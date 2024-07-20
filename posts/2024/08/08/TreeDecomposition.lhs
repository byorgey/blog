---
title: 'Competitive Programming in Haskell: tree path decomposition, part II'
categories: competitive programming,haskell
tags: challenge,Kattis,number theory,tree,path,decomposition
katex: true
---

In [a previous
post](https://byorgey.github.io/blog/posts/2024/07/11/cpih-factor-full-tree.html)
I discussed the first half of my solution to [Factor-Full
Tree](https://open.kattis.com/problems/factorfulltree).  In this post,
I will demonstrate how to *decompose a tree into disjoint paths*.
^[{-} Technically, we should clarify that we are looking for *directed*
paths in a rooted tree, that is, paths that only proceed down the
tree.  One could also ask about decomposing an unrooted tree into
disjoint undirected paths; I haven't thought about how to do that in
general but intuitively I expect it is not too much more difficult.] For
this particular problem, we want to decompose a tree into
*maximum-length* paths (*i.e.* we start by taking the longest possible
path, then take the longest path from what remains, and so on); I will call
this the *max-chain decomposition* (I don't know if there is a
standard term).  However, there are other types of path
decomposition, such as heavy-light decomposition, so we will try to
keep the decomposition code somewhat generic.

Preliminaries
-------------

This post is literate Haskell; you can [find the source code on GitHub](https://github.com/byorgey/blog/blob/main/posts/2024/08/08/TreeDecomposition.lhs).
We begin with some language pragmas and imports.

> {-# LANGUAGE ImportQualifiedPost #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE TupleSections #-}
>
> module TreeDecomposition where
>
> import Control.Arrow ((>>>), (***))
> import Data.Bifunctor (second)
> import Data.ByteString.Lazy.Char8 (ByteString)
> import Data.ByteString.Lazy.Char8 qualified as BS
> import Data.List (sortBy)
> import Data.List.NonEmpty (NonEmpty)
> import Data.List.NonEmpty qualified as NE
> import Data.Map (Map, (!), (!?))
> import Data.Map qualified as M
> import Data.Ord (Down(..), comparing)
> import Data.Tree (Tree(..), foldTree)
> import Data.Tuple (swap)
>
> import ScannerBS

[See here for the `ScannerBS` module](https://github.com/byorgey/comprog-hs/blob/master/ScannerBS.hs).

Generic path decomposition
--------------------------

Remember, our goal is to split up a tree into a collection of linear
paths; that is, in general, something like this:

```{.diagram width=400 height=400 caption=""}
import qualified Data.List.NonEmpty as NE
import Data.Tree
-- import Diagrams.Backend.Rasterific.CmdLine
-- import Diagrams.Prelude
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Layout.Tree
import qualified Data.Map as M
import Data.Map (Map, (!))
import Control.Monad.State
import Data.Ord
import Data.List (sortBy)
import Control.Category ((>>>))
import Control.Arrow ((***))
import Data.Bifunctor (second)

lf :: Tree ()
lf = Node () []

chain :: Int -> Tree ()
chain 0 = lf
chain n = Node () [chain (n-1)]

chainWith :: Int -> [(Int, Tree ())] -> Tree ()
chainWith 0 _ = lf
chainWith n [] = chain n
chainWith n ((k,t):ts)
  | n == k = Node () [chainWith (n-1) ts, t]
  | otherwise = Node () [chainWith (n-1) ((k,t):ts)]

spine :: Int -> Tree ()
spine 0 = lf
spine n = Node () [spine (n-1), lf]

t :: Tree Int
t = labelUnique $ Node () [chainWith 10 [(8, chainWith 5 [(3, chain 1)])], chainWith 9 [(6, chain 2)]]

labelUnique :: Tree () -> Tree Int
labelUnique t = evalState (traverse (const inc) t) 0
  where
    inc = get >>= \s -> put (s+1) >> return s

drawPaths :: Ord a => [[a]] -> Tree (a, P2 Double) -> Diagram B
drawPaths ps t = foldMap drawPath ps
  where
    locOf = M.fromList $ flatten t
    drawPath = lw none . fc lightblue . translate (0.5 ^& (-0.5)) . strokeP . expandTrail' (with & expandJoin .~ LineJoinRound & expandCap .~ LineCapRound) 1 . fromVertices . map (locOf M.!)

dia = mconcat
  [ renderTree (\n -> circle 0.5 # fc white) (~~) laidOutTree
  , drawPaths ps laidOutTree
  ]
  where
    ps = map (NE.toList . fmap snd) $ maxChainDecomposition t
    laidOutTree = symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t

maxChainDecomposition :: Tree a -> [NE.NonEmpty (Height, a)]
maxChainDecomposition =
  labelHeight >>>
  pathDecomposition (const (selectMaxBy (comparing (fst . rootLabel)))) >>>
  sortBy (comparing (Down . fst . NE.head))

selectMaxBy :: (a -> a -> Ordering) -> [a] -> Maybe (a, [a])
selectMaxBy _ [] = Nothing
selectMaxBy cmp (a : as) = case selectMaxBy cmp as of
  Nothing -> Just (a, [])
  Just (b, bs) -> case cmp a b of
    LT -> Just (b, a : bs)
    _ -> Just (a, b : bs)

type Height = Int

labelHeight :: Tree a -> Tree (Height, a)
labelHeight = foldTree node
 where
  node a ts = case ts of
    [] -> Node (0, a) []
    _ -> Node (1 + maximum (map (fst . rootLabel) ts), a) ts

type SubtreeSelector a = a -> [Tree a] -> Maybe (Tree a, [Tree a])

pathDecomposition :: SubtreeSelector a -> Tree a -> [NE.NonEmpty a]
pathDecomposition select = go
 where
  go = selectPath select >>> second (concatMap go) >>> uncurry (:)

selectPath :: SubtreeSelector a -> Tree a -> (NE.NonEmpty a, [Tree a])
selectPath select = go
 where
  go (Node a ts) = case select a ts of
    Nothing -> (NE.singleton a, ts)
    Just (t, ts') -> ((a NE.<|) *** (ts' ++)) (go t)
```

What do we need in order to specify a decomposition of a
tree into disjoint paths this way?  Really, all we need is to choose *at most
one linked child* for each node.  In other words, at every node we can
choose to continue the current path into a single child node (in which
case all the other children will start their own new paths), or we
could choose to terminate the current path (in which case every child
will be the start of its own new path).  We can represent such a
choice with a function of type

> type SubtreeSelector a = a -> [Tree a] -> Maybe (Tree a, [Tree a])

which takes as input the value at a node and the list of all the
subtrees, and possibly returns a selected subtree along with the list of remaining
subtrees.^[{-} Of course, there is nothing in the
type that actually requires a `SubtreeSelector` to return one of the
trees from its input paired with the rest, but nothing we will do
depends on this being true. In fact, I expect there may be some
interesting algorithms obtainable by running a "path decomposition"
with a "selector" function that actually makes up new trees instead of just
selecting one, similar to [the `chop` function](https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split.html#v:chop).]

Given such a subtree selection function, a generic path decomposition
function will then take a tree and turn it into a list of non-empty
paths:^[{-} We could also imagine wanting information about the parent of each
path, and a mapping from tree nodes to some kind of path ID, but we
will keep things simple for now.]

> pathDecomposition :: SubtreeSelector a -> Tree a -> [NonEmpty a]

Implementing `pathDecomposition` is a nice exercise; you might like to
try it yourself!  You can find my implementation at the end of this
blog post.

Max-chain decomposition
-----------------------

Now, let's use our generic path decomposition to implement a max-chain
decomposition.  At each node we want to select the *tallest* subtree;
in order to do this efficiently, we can first annotate each tree node with
its height, via a straightforward [tree fold](https://hackage.haskell.org/package/containers-0.7/docs/Data-Tree.html#v:foldTree):

> type Height = Int
>
> labelHeight :: Tree a -> Tree (Height, a)
> labelHeight = foldTree node
>  where
>   node a ts = case ts of
>     [] -> Node (0, a) []
>     _ -> Node (1 + maximum (map (fst . rootLabel) ts), a) ts

Our subtree selection function can now select the subtree with the
largest `Height` annotation.  Instead of implementing this directly,
we might as well make a generic function for selecting the "best"
element from a list (we will reuse it later):

> selectMaxBy :: (a -> a -> Ordering) -> [a] -> Maybe (a, [a])
> selectMaxBy _ [] = Nothing
> selectMaxBy cmp (a : as) = case selectMaxBy cmp as of
>   Nothing -> Just (a, [])
>   Just (b, bs) -> case cmp a b of
>     LT -> Just (b, a : bs)
>     _ -> Just (a, b : bs)

We can now put the pieces together to implement max-chain
decomposition.  We first label the tree by height, then do a path
decomposition that selects the tallest subtree at each node.  We leave
the height annotations in the final output since they might be
useful---for example, we can tell how long each path is just by
looking at the `Height` annotation on the first element. If we don't
need them we can easily get rid of them later.  We also sort by
descending `Height`, since getting the longest chains first was kind
of the whole point.

> maxChainDecomposition :: Tree a -> [NonEmpty (Height, a)]
> maxChainDecomposition =
>   labelHeight >>>
>   pathDecomposition (const (selectMaxBy (comparing (fst . rootLabel)))) >>>
>   sortBy (comparing (Down . fst . NE.head))

Factor-full tree solution
-------------------------

To flesh this out into a full solution to [Factor-Full
Tree](https://open.kattis.com/problems/factorfulltree), after
computing the chain decomposition we need to assign prime factors to
the chains.  From those, we can compute the value for each node if we
know which chain it is in and the value of its parent.  To this end,
we will need one more function which computes a `Map` recording the
parent of each node in a tree.  Note that if we already know all the
edges in a given edge list are oriented the same way, we can build
this much more simply as *e.g.* `map swap >>> M.fromList`; but when
(as in general) we don't know which way the edges should be oriented
first, we might as well first build a `Tree a` via DFS with
`edgesToTree` and then construct the `parentMap` like this afterwards.

> parentMap :: Ord a => Tree a -> Map a a
> parentMap = foldTree node >>> snd
>  where
>   node :: Ord a => a -> [(a, Map a a)] -> (a, Map a a)
>   node a b = (a, M.fromList (map (,a) as) <> mconcat ms)
>    where
>     (as, ms) = unzip b

Finally, we can solve Factor-Full tree.  Note that some code from my
[previous blog
post](https://byorgey.github.io/blog/posts/2024/07/11/cpih-factor-full-tree.html)
is needed as well, and is included at the end of the post for
completeness.  Once we compute the max chain decomposition and the
prime factor for each node, we use a [lazy recursive
`Map`](https://byorgey.github.io/blog/posts/2023/04/11/competitive-programming-in-haskell-topsort-via-laziness.html)
to compute the value assigned to each node.

> solve :: TC -> [Int]
> solve TC{..} = M.elems assignment
>   where
>     -- Build the tree and compute its parent map
>     t = edgesToTree Node edges 1
>     parent = parentMap t
>
>     -- Compute the max chain decomposition, and use it to assign a prime factor
>     -- to each non-root node
>     paths :: [[Node]]
>     paths = map (NE.toList . fmap snd) $ maxChainDecomposition t
>
>     factor :: Map Node Int
>     factor = M.fromList . concat $ zipWith (\p -> map (,p)) primes paths
>
>     -- Compute an assignment of each node to a value, using a lazy map
>     assignment :: Map Node Int
>     assignment = M.fromList $ (1,1) : [(v, factor!v * assignment!(parent!v)) | v <- [2..n]]

For an explanation of this code for `primes`, see [this old blog post](https://byorgey.github.io/blog/posts/2020/02/07/competitive-programming-in-haskell-primes-and-factoring.html).

> primes :: [Int]
> primes = 2 : sieve primes [3 ..]
>  where
>   sieve (p : ps) xs =
>     let (h, t) = span (< p * p) xs
>      in h ++ sieve ps (filter ((/= 0) . (`mod` p)) t)

Bonus: heavy-light decomposition
--------------------------------

We can easily use our generic path decomposition to compute a [heavy-light
decomposition](https://cp-algorithms.com/graph/hld.html) as well:

> type Size = Int
>
> labelSize :: Tree a -> Tree (Size, a)
> labelSize = foldTree $ \a ts -> Node (1 + sum (map (fst . rootLabel) ts), a) ts
>
> heavyLightDecomposition :: Tree a -> [NonEmpty (Size, a)]
> heavyLightDecomposition =
>   labelSize >>>
>   pathDecomposition (const (selectMaxBy (comparing (fst . rootLabel))))

I plan to write about this in a future post.

Leftover code
-------------

Here's my implementation of `pathDecomposition`; how did you do?

> pathDecomposition select = go
>  where
>   go = selectPath select >>> second (concatMap go) >>> uncurry (:)
>
> selectPath :: SubtreeSelector a -> Tree a -> (NonEmpty a, [Tree a])
> selectPath select = go
>  where
>   go (Node a ts) = case select a ts of
>     Nothing -> (NE.singleton a, ts)
>     Just (t, ts') -> ((a NE.<|) *** (ts' ++)) (go t)

We also include some input parsing and tree-building code from [last time](https://byorgey.github.io/blog/posts/2024/07/11/cpih-factor-full-tree.html).

> main :: IO ()
> main = BS.interact $ runScanner tc >>> solve >>> map (show >>> BS.pack) >>> BS.unwords
>
> type Node = Int
> data TC = TC { n :: Int, edges :: [(Node, Node)] }
>   deriving (Eq, Show)
>
> tc :: Scanner TC
> tc = do
>   n <- int
>   edges <- (n - 1) >< pair int int
>   return TC{..}
>
> edgesToMap :: Ord a => [(a, a)] -> Map a [a]
> edgesToMap = concatMap (\p -> [p, swap p]) >>> dirEdgesToMap
>
> dirEdgesToMap :: Ord a => [(a, a)] -> Map a [a]
> dirEdgesToMap = map (second (: [])) >>> M.fromListWith (++)
>
> mapToTree :: Ord a => (a -> [b] -> b) -> Map a [a] -> a -> b
> mapToTree nd m root = dfs root root
>  where
>   dfs parent root = nd root (maybe [] (map (dfs root) . filter (/= parent)) (m !? root))
>
> edgesToTree :: Ord a => (a -> [b] -> b) -> [(a, a)] -> a -> b
> edgesToTree nd = mapToTree nd . edgesToMap
