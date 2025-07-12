---
title: 'Competitive programming in Haskell: sparse tables'
categories: Haskell,competitive programming
katex: true
published: 2025-07-12T00:00:00Z
tags: monoid,semigroup,idempotent,range,query,sum,sparse,table
---

Continuing a [series of
posts](https://byorgey.github.io/blog/posts/2025/06/23/range-queries-classified.html)
on techniques for calculating *range queries*, today I will present
the *sparse table* data structure.

Motivation
----------

In my [previous
post](https://byorgey.github.io/blog/posts/2025/06/27/prefix-sums.html),
we saw that if we have a static sequence and a binary operation with a
*group* structure (*i.e.* every element has an inverse), we can
precompute a prefix sum table in $O(n)$ time, and then answer
arbitrary range queries in $O(1)$ time.

What if we don't have inverses?  We can't use prefix sums, but can we
do something else that still allows us to answer range queries in
$O(1)$?  Clearly, one thing we could do would be to construct an $n
\times n$ table storing the answer to *every possible* range query,
that is, $Q[i,j]$ would store the value of the range $a_i \diamond
\dots \diamond a_j$. Then we could just look up the answer to any
range query in $O(1)$.  Naively computing the value of each $Q[i,j]$
would take $O(n)$ time, for a total of $O(n^3)$ time to fill in each
of the entries in the table^[We only have to fill in $Q[i,j]$ where
$i < j$, but this is still about $n^2/2$ entries.], but it's not too
hard to fill in the table in $O(n^2)$ total time, spending only $O(1)$
to fill in each entry---I'll leave this to you as an exercise.

However, $O(n^2)$ is often too big.  Can we do better?  More
generally, we are looking for a particular subset of range queries to
precompute, such that the total number is asymptotically less than
$n^2$, XXX.  In the case of a group structure, we were able to compute
only the $O(n)$ prefix queries, *i.e.* range queries of the form
$[1,k]$, and compute the value of an arbitrary range using two
prefixes, via subtraction.

A sparse
table is a particularly simple way to cut down the precomputation time
and space to only $O(n \lg n)$---*if* the operation is *idempotent*,
that is, $x \diamond x = x$.  The most common examples of idempotent
binary operations are $\min$ and $\max$, but there are others---for
example, bitwise OR.

Sparse table
------------

So, how does a sparse table work?  The basic idea is that we store a
series of "levels", where level $i$ stores range sums of length $2^i$.
So level $0$ stores "range sums of length $1$"---that is, the elements
of the original sequence; level $1$ stores range sums of length $2$;
level $2$ stores range sums of length $4$; and so on. Formally,
$T[i,j]$ stores the value of the range of length $2^i$ starting at
index $j$.  That is,

$T[i,j] = a_j \diamond \dots \diamond a_{j+2^i-1}$.

We can see that $i$ only needs to range from $0$ up to $\lfloor \lg n
\rfloor$---above that and the stored range sums would be larger than
the entire sequence.  So this table has size $O(n \lg n)$.

Two important questions remain: how do we compute this table in the
first place? And once we have it, how do we use it to answer arbitrary
range queries in $O(1)$?

The first is easy: XXX each range sum on level $i$, of length $2^i$, is the
combination of two range sums from level $i-1$.

```{.diagram width=400 height=400 caption=""}
dia = circle 1 # fc blue
```

Haskell code
------------

```haskell
module IdempotentSemigroup where

import Data.Bits
import Data.Semigroup

-- | An idempotent semigroup is one where the binary operation
--   satisfies the law @x <> x = x@ for all @x@.
class Semigroup m => IdempotentSemigroup m

instance Ord a => IdempotentSemigroup (Min a)
instance Ord a => IdempotentSemigroup (Max a)
instance IdempotentSemigroup All
instance IdempotentSemigroup Any
instance IdempotentSemigroup Ordering
instance IdempotentSemigroup ()
instance IdempotentSemigroup (First a)
instance IdempotentSemigroup (Last a)
instance Bits a => IdempotentSemigroup (And a)
instance Bits a => IdempotentSemigroup (Ior a)
```

```haskell
{-# LANGUAGE TupleSections #-}

module SparseTable where

import Data.Array (Array, array, (!))
import Data.Bifunctor (first)
import Data.Bits
import IdempotentSemigroup

newtype SparseTable m = SparseTable (Array (Int, Int) m)
  deriving (Show)

-- | Logarithm base 2, rounded down to the nearest integer.  Computed
--   efficiently using primitive bitwise instructions.
lg :: Int -> Int
lg n = finiteBitSize n - 1 - countLeadingZeros n

-- | Construct a sparse table which can answer range queries over the
--   given list in $O(1)$ time.  Constructing the sparse table takes
--   $O(n \lg n)$ time and space, where $n$ is the length of the list.
fromList :: IdempotentSemigroup m => [m] -> SparseTable m
fromList ms = SparseTable st
 where
  n = length ms
  lgn = lg n

  st =
    array ((0, 0), (lgn, n - 1)) $
      (map (first (0,)) $ zip [0 ..] ms)
        ++ [ ((i, j), st ! (i - 1, j) <> st ! (i - 1, j + 1 !<<. (i - 1)))
           | i <- [1 .. lgn]
           , j <- [0 .. n - 1 !<<. i]
           ]

-- | \$O(1)$. @range st l r@ computes the range query which is the
--   @sconcat@ of all the elements from index @l@ to @r@ (inclusive).
range :: IdempotentSemigroup m => SparseTable m -> Int -> Int -> m
range (SparseTable st) l r = st ! (i, l) <> st ! (i, r - (1 !<<. i) + 1)
 where
  i = lg (r - l + 1)

```

Applications
------------

XXX LCA via Euler tour + RMQ

Practice problems
-----------------

Want to practice?  Here are a few problems that can be solved using
techniques discussed in this post:

XXX
