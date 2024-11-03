---
title: 'Competitive Programming in Haskell: Union-Find, part II'
categories: competitive programming,haskell
tags: challenge,Kattis,union-find
katex: true
---

In my [previous
post](https://byorgey.github.io/blog/posts/2024/11/02/UnionFind.html)
I explained how to implement a reasonably efficient *union-find* data
structure in Haskell, and challenged you to solve a couple Kattis
problems.

Before going on to explain my solutions to those problems, I want to
highlight some things from a [comment by Derek
Elkins](https://byorgey.github.io/blog/posts/2024/11/02/UnionFind.html#isso-1971)
and a related [blog post by Philip
Zucker](https://www.philipzucker.com/union-find-groupoid/).  The first
is that instead of (or in addition to) annotating each set with a
value from a commutative semigroup, we can also annotate the *edges*
between nodes with elements from a group (or, more generally, a
groupoid).  The idea is that each edge records some information about,
or evidence for, the *relationship* between the endpoints of the edge.
To compute information about the relationship between two arbitrary
nodes in the same set, we can compose elements along the path between
them.  This is a nifty idea---I have never seen it used for a
competitive programming problem but it certainly could be.  And of
course it has "real" applications beyond competitive programming as
well.

The other idea to highlight is that instead of thinking in terms of
*sets*, what we are really doing is building an *equivalence
relation*. XXX

XXX Try updating to something like Rem's algorithm & test it out on
Kattis problems.

Duck Journey
------------

In [Duck Journey](https://open.kattis.com/problems/andvag), we are
essentially given a graph with edges labelled by bitstrings, where
edges along a path are combined using bitwise OR.  We are then asked
to find the *greatest* possible value of a path between two given
vertices, assuming that we are allowed to retrace our steps as much as
we want.^[Incidentally, if we are *not* allowed to retrace our steps,
this problem probably becomes NP-hard.] If we can retrace our steps,
then on our way from A to B we might as well visit every edge in the
entire connected component, so this problem is not really
about path-finding at all.  It boils down to two things: (1) being
able to quickly test whether two given vertices are in the same
connected component or not, and (2) computing the bitwise OR of all
the edge labels in each connected component.

One way to solve this would be to first use some kind of graph
traversal, like DFS, to find the connected components and build a map
from vertices to component labels; then partition the edges by
component and take the bitwise OR of all the edge weights in each
component.  To answer queries we could first look up the component
label of the two vertices; if the labels are the same then we look up
the total weight for that component.

This works, and is in some sense the most "elemantary" solution, but
it requires building some kind of graph data structure, storing all
the edges in memory, doing the component labelling via DFS and
building another map, and so on.  An alternative solution is to use a
union-find structure with a bitstring annotation for each set: as we
read in the edges in the input, we simply union the endpoints of the
edge, and then combine the edge bitstring with the bitstring for the
resulting component.

First, some imports and the top-level `main` function. ([See here for the `ScannerBS` module](https://github.com/byorgey/comprog-hs/blob/master/ScannerBS.hs).)

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Category ((>>>))
import Control.Monad.ST
import Data.Bits
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS

import ScannerBS
import UnionFind qualified as UF

main = BS.interact $ runScanner tc >>> solve >>> format

format :: [Maybe Int] -> ByteString
format = map (maybe "-1" (show >>> BS.pack)) >>> BS.unlines
```

Next, some data types to represent the input, and a `Scanner` to read
it.

```haskell
newtype Filter = Filter Int
  deriving (Eq, Show)

instance Semigroup Filter where
  Filter x <> Filter y = Filter (x .|. y)

filterSize :: Filter -> Int
filterSize (Filter f) = popCount f

data Channel = Channel UF.Node UF.Node Filter deriving (Eq, Show)
data TC = TC {n :: !Int, channels :: [Channel], queries :: [(Int, Int)]}
  deriving (Eq, Show)

tc :: Scanner TC
tc = do
  n <- int
  m <- int
  q <- int
  channels <- m >< (Channel <$> int <*> int <*> (Filter <$> int))
  queries <- q >< pair int int
  return TC {..}
```

Finally, here's the solution itself: process each channel with a
union-find structure, then process queries.  It's relatively
straightforward; the annoying thing, of course, is that it all has to
be in the `ST` monad.

```
solve :: TC -> [Maybe Int]
solve TC {..} = runST $ do
  uf <- UF.new (n + 1) (Filter 0)
  mapM_ (addChannel uf) channels
  mapM (answer uf) queries

addChannel :: UF.UnionFind s Filter -> Channel -> ST s ()
addChannel uf (Channel a b f) = do
  UF.union uf a b
  UF.updateAnn uf a f

answer :: UF.UnionFind s Filter -> (Int, Int) -> ST s (Maybe Int)
answer uf (a, b) = do
  c <- UF.connected uf a b
  case c of
    False -> pure Nothing
    True -> Just . filterSize <$> UF.getAnn uf a
```

Inventing Test Data
-------------------

In [Inventing Test Data](https://open.kattis.com/problems/inventing),
we are given a tree $T$ with weights on its edges, and asked to find the
minimum possible weight of a complete graph for which $T$ is the
[minimum spanning tree](XXX).

```haskell
main = BS.interact $ runScanner (numberOf tc) >>> map (solve >>> showB) >>> BS.unlines
```

```haskell
data Edge = Edge {a :: !Int, b :: !Int, w :: !Integer}
  deriving (Eq, Show)

instance Ord Edge where
  compare = comparing w

data TC = TC {n :: !Int, edges :: [Edge]}
  deriving (Eq, Show)

tc :: Scanner TC
tc = do
  n <- int
  edges <- (n - 1) >< (Edge <$> int <*> int <*> integer)
  return TC {..}
```

Finally, the solution proper: we `sort` the edges and process them
from smallest to biggest; for each edge we update an accumulator
according to the formula discussed above.  Since we're already tied to
the `ST` monad anyway, we might as well keep the accumulator in a
mutable `STRef` cell.

```haskell
solve :: TC -> Integer
solve TC {..} = runST $ do
  uf <- UF.new (n + 1)
  total <- newSTRef (0 :: Integer)
  mapM_ (processEdge uf total) (sort edges)
  readSTRef total

processEdge :: UF.UnionFind s -> STRef s Integer -> Edge -> ST s ()
processEdge uf total (Edge {..}) = do
  modifySTRef' total (+ w)
  sa <- UF.size uf a
  sb <- UF.size uf b
  modifySTRef' total (+ (fromIntegral sa * fromIntegral sb - 1) * (w + 1))
  UF.union uf a b
```
