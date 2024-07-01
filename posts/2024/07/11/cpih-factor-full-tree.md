---
title: 'Competitive Programming in Haskell: tree path decomposition, part I'
categories: competitive programming,haskell
tags: challenge,Kattis,number theory,tree,path,decomposition
katex: true
---

In [a previous
post](https://byorgey.github.io/blog/posts/2024/05/28/competitive-programming-in-haskell-two-problems.html)
I challenged you to solve [Factor-Full
Tree](https://open.kattis.com/problems/factorfulltree).  In this
problem, we are given an unlabelled rooted tree, and asked to create a *divisor
labelling*.  That is, we must label the vertices with positive
integers in such a way that $v$ is an ancestor of $u$ if and only if
$v$'s label evenly divides $u$'s label.

For example, here is a tree with a divisor labelling:

^[{-} Divisor labelling of a tree]
```{.diagram width=400 height=400 caption=""}
import Data.Tree
import Diagrams.TwoD.Layout.Tree

t = Node 1 [Node 5 [], Node 7 [Node 14 [], Node 21 [Node 63 []], Node 49 []], Node 6 [Node 12 []]]

dia = renderTree (\n -> text (show n) <> circle 1 # fc white) (~~) (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t)
```

The interesting point (though irrelevant to solving the problem) is
that this is a method for encoding a tree as a set of integers:
because $v$ is an ancestor of $u$ *if and only if* $v$'s label divides
$u$'s, all the information about the tree's structure is fully
contained in the set of labels.  For example, if we simply write
down the set $\{1, 5, 6, 7, 12, 14, 21, 49, 63\}$, it is possible to
fully reconstruct the above tree from this set.^[{-} Note that we
consider trees equivalent up to reordering of siblings, that is, each
node has a *bag*, not a *list*, of children.] This is not a
particularly *efficient* way to encode a tree, but it is certainly
interesting!

Basic setup
-----------

First, some basic setup.^[{-} [See here](https://byorgey.github.io/blog/posts/2019/05/22/competitive-programming-in-haskell-scanner.html) for the `Scanner`
abstraction, and
[here](https://byorgey.github.io/blog/posts/2019/04/24/competitive-programming-in-haskell-basic-setup.html)
for the basics of how I organize solutions.] The first line of
input specifies the number of nodes $N$, and after that there are
$N-1$ lines, each specifying a single undirected edge.

```haskell
import Control.Category ((>>>))
import Data.Bifunctor (second)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Tuple (swap)

main = C.interact $ runScanner tc >>> solve >>> format

data TC = TC { n :: Int, edges :: [Edge] }
  deriving (Eq, Show)

tc :: Scanner TC
tc = do
  n <- int
  edges <- (n - 1) >< pair int int
  return TC{..}

format :: [Integer] -> ByteString
format = map showB >>> C.unwords
```

We are guaranteed that the edges describe a tree; next we will
actually build a tree data structure from the input.

Building trees
--------------

There are many
similar problems which specify a tree structure by giving a list of
edges, so it's worthwhile trying to write some generic code to
transform such an input into an actual tree. In an imperative language
we would do this by building a map from each node to its neighbors,
then doing a DFS to orient the tree.  Our Haskell code will be
similar, except building the map and doing a DFS will both be
one-liners!

First, a function to turn a list of undirected edges into a `Map`
associating each vertex to all its neighbors.  It's convenient to
decompose this into a function to turn a list of *directed* edges into
a `Map`, and a function to duplicate and swap each pair.  We won't
need `dirEdgesToMap` for this problem, but we can certainly imagine
wanting it elsewhere.
```haskell
edgesToMap :: Ord a => [(a, a)] -> Map a [a]
edgesToMap = concatMap (\p -> [p, swap p]) >>> dirEdgesToMap

dirEdgesToMap :: Ord a => [(a, a)] -> Map a [a]
dirEdgesToMap = map (second (: [])) >>> M.fromListWith (++)
```

Next, we can turn such a neighbor `Map` into a tree.  Rather than
returning a literal `Tree` data structure, it's convenient to
incorporate a tree fold: that is, given a function `a -> [b] -> b`, a neighbor
map, and a root node, we fold over the whole tree and return the
resulting `b` value.  (Of course, if we want an actual `Tree` we can use
`mapToTree Node`.)  We can also compose these into a single function `edgesToTree`.
```haskell
mapToTree :: Ord a => (a -> [b] -> b) -> Map a [a] -> a -> b
mapToTree nd m root = dfs root root
 where
  dfs parent root = nd root (maybe [] (map (dfs root) . filter (/= parent)) (m !? root))

edgesToTree :: Ord a => (a -> [b] -> b) -> [(a, a)] -> a -> b
edgesToTree nd = mapToTree nd . edgesToMap
```

Inventing divisor labellings
----------------------------

So how do we create a divisor labelling for a given tree?  Clearly, we
might as well choose the root to have label $1$, and every time we
descend from a parent to a child, we must multiply by some integer,
which might as well be a prime.  Of course, we need to multiply by a
*different* prime for each sibling.  We might at first imagine simply
multiplying by 2 for each (arbitrarily chosen) leftmost child, 3 for
each second child, 5 for each third child, and so on, but this does
not work---the second child of the first child ends up with the same
label as the first child of the second child, and so on.

Each node $u$'s label is some prime $p$ times its parent's label; call
$p$ the *factor* of node $u$.  It is OK for one child of $u$ to also
have factor $p$, but the other children must get different factors.
To be safe, we can give each additional child a new *globally unique*
prime factor.  This is not always necessary---in some cases it can be
OK to reuse a factor if it does not lead to identically numbered
nodes---but it is certainly sufficient.  As an example, below is a
divisor labelling of the example tree from before, via this scheme.
Each edge is labelled with the factor of its child.

^[{-} Divisor labelling of a tree with consecutive primes]
```{.diagram width=400 height=400 caption=""}
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import qualified Diagrams.Color.XKCD as XKCD

t = Node 1 [Node 2 [], Node 3 [Node 9 [], Node 15 [Node 75 []], Node 21 []], Node 11 [Node 121 []]]

dia = renderTree' (\n -> text (show n) <> circle 1 # fc white) labelledEdge (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t)

labelledEdge :: (Int, P2 Double) -> (Int, P2 Double) -> Diagram B
labelledEdge (n,x) (m,y) = mconcat
  [ mconcat
    [ text (show (m `div` n)) # fontSizeL 0.6
    , square 0.7 # lw thin # fcA XKCD.reallyLightBlue
    ]
    # moveTo (centroid [x,y])
  , x ~~ y
  ]
```

Notice how we use $2$ for the first child of the root, and $3$ for the
next child.  $3$'s first child can also use a factor of $3$, yielding
a label of $3^2 = 9$.  $3$'s next child uses a new, globally unique
prime $5$, and its third child uses $7$; the final child of $1$ uses
the next available prime, $11$.

We can code this up via a simple stateful traversal of the tree. (For
`primes`, see [this
post](https://byorgey.github.io/blog/posts/2020/02/07/competitive-programming-in-haskell-primes-and-factoring.html).)
It's a bit fiddly since we have to switch to the next prime *between*
consecutive children, but not *after* the last child.

```haskell
primes :: [Integer]
primes = 2 : sieve primes [3 ..]
 where
  sieve (p : ps) xs =
    let (h, t) = span (< p * p) xs
     in h ++ sieve ps (filter ((/= 0) . (`mod` p)) t)

curPrime :: State [Integer] Integer
curPrime = gets head

nextPrime :: State [Integer] ()
nextPrime = modify tail

labelTree :: Tree a -> Tree (Integer, a)
labelTree = flip evalState primes . go 1
 where
  go :: Integer -> Tree a -> State [Integer] (Tree (Integer, a))
  go x (Node a ts) = Node (x, a) <$> labelChildren x ts

  labelChildren :: Integer -> [Tree a] -> State [Integer] [Tree (Integer, a)]
  labelChildren _ [] = pure []
  labelChildren x (t : ts) = do
    p <- curPrime
    t' <- go (x * p) t
    case ts of
      [] -> pure [t']
      _ -> do
        nextPrime
        (t' :) <$> labelChildren x ts
```

There is a bit of additional glue code we need get the parsed tree
from the input, apply `labelTree`, and then print out the node
labels in order.  However, I'm not going to bother showing it,
because---this solution is not accepted!  It fails with a WA (Wrong
Answer) verdict.  What gives?

Keeping things small
--------------------

The key is one of the last sentences in the problem statement, which I
haven't mentioned so far: all the labels in our output must be at most
$10^{18}$.  Why is this a problem?  Multiplying by primes over and
over again, it's not hard to get rather large numbers.  For example,
consider the tree below:

^[{-} Tree for which our naïve scheme generates labels that are too large]
```{.diagram width=400 height=400 caption=""}
import Data.Tree
import Diagrams.TwoD.Layout.Tree

lf :: Tree ()
lf = Node () []

chain :: Int -> Tree ()
chain 0 = lf
chain n = Node () [chain (n-1)]

t :: Tree ()
t = Node () (replicate 9 lf ++ [chain 12])

dia = renderTree (\n -> circle 0.5 # fc black) (~~) (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t)
```

Under our scheme, the root gets label $1$, and the children of the
root get consecutive primes $2, 3, 5, \dots, 29$.  Then the nodes
in the long chain hanging off the last sibling get labels $29^2, 29^3,
\dots, 29^{13}$, and $29^{13}$ is too big---in fact, it is
approximately $10^{19}$.  And this tree has only 23 nodes; in general
the input can have up to 60.

Of course, $29$ was a poor choice of factor for such a long chain---we
should have instead labelled the long chain with powers of,
say, 2. Notice that if we have a "tree" consisting of a single long
chain of 60 nodes (and you can bet this is one of the secret test
inputs!), we just barely get by labelling it with powers of two from
$2^0$ up to $2^{59}$: in fact $2^{59} < 10^{18} < 2^{60}$.  So in
general, we want to find a way to label long chains with small primes,
and reserve larger primes for shorter chains.

Attempt 1: sorting by height
----------------------------

One obvious approach is to simply sort the children at each node by
decreasing height, before traversing the tree to assign prime
factors.  This handles the above example correctly, since the long
chain would be sorted to the front and assigned the factor 2.
However, this does not work in general!  It can still fail to assign
the smallest primes to the longest chains.  As a simple example,
consider this tree, in which the children of every node are already
sorted by decreasing height from left to right:

^[{-} Tree for which sorting by height first does not work]
```{.diagram width=400 height=400 caption=""}
import Data.Tree
import Diagrams.TwoD.Layout.Tree

lf :: Tree ()
lf = Node () []

chain :: Int -> Tree ()
chain 0 = lf
chain n = Node () [chain (n-1)]

spine :: Int -> Tree ()
spine 0 = lf
spine n = Node () [spine (n-1), lf]

t :: Tree ()
t = Node () [spine 12, chain 11]

dia = renderTree (\n -> circle 0.5 # fc black) (~~) (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t)
```

The straightforward traversal algorithm indeed assigns powers of 2 to
the left spine of the tree, but it then assigns 3, 5, 7, and so on to
all the tiny spurs hanging off it. So by the time we get to other long
chain hanging off the root, it is assigned powers of $43$, which are
too big.  In fact, we want to assign powers of 2 to the left spine,
powers of 3 to the chain on the right, and then use the rest of the
primes for all the short spurs.  But this sort of "non-local"
labelling means we can't assign primes via a tree traversal.

To drive this point home, here's another example tree.  This one is
small enough that it probably doesn't matter too much how we label it,
but it's worth thinking about how to label the longest chains with the
smallest primes.  I've drawn it in a "left-leaning" style to further
emphasize the different chains that are involved.

^[{-} Tree for which XXX]
```{.diagram width=400 height=400 caption=""}
import Data.Tree
import Diagrams.TwoD.Layout.Tree

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

t :: Tree ()
t = Node () [chainWith 10 [(8, chainWith 5 [(3, chain 1)])], chainWith 9 [(6, chain 2)]]

drawLeftLeaning :: (a -> Diagram B) -> (P2 Double -> P2 Double -> Diagram B)
  -> Tree a -> Diagram B
drawLeftLeaning n e (Node a ts) = localize $ vsep 4
  [ n a # named "root"
  , hsep 4 (zipWith named [1 :: Int ..] (map (drawLeftLeaning n e) ts))
  ]
  # withName "root" ( \root ->
    withNames [1 .. length ts] ( \tns ->
      applyAll (map (beneath . e (location root) . location) tns)
    )
    )

dia = drawLeftLeaning (\n -> circle 0.5 # fc black) (~~) t
```

In fact, we want to assign the factor 2 to the long chain on the left;
then the factor 3 to the second-longest chain, in the fourth column;
then 5 to the length-6 chain in the second column; 7 to the length-3
chain all the way on the right; and finally 11 to the smallest chain, in column 3.

In general, then, we want a way to *decompose* an arbitrary tree into
chains, where we repeatedly identify the longest chain, remove it from
consideration, and then identify the longest chain from the remaining
nodes, and so on.  Once we have decomposed a tree into chains, it will
be a relatively simple matter to sort the chains by length and assign
consecutive prime factors.

This decomposition occasionally comes in handy (for example, see
[Floating
Formation](https://open.kattis.com/problems/floatingformation) and
[Dead-End Detector](https://open.kattis.com/problems/deadend)), and
belongs to a larger family of important tree decomposition techniques
such as [heavy-light
decomposition](https://cp-algorithms.com/graph/hld.html). Next time,
I'll demonstrate how to implement such tree decompositions in Haskell!
