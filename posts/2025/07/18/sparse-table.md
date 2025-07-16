---
title: 'Competitive programming in Haskell: sparse tables'
categories: Haskell,competitive programming
katex: true
tags: monoid,semigroup,idempotent,range,query,sum,sparse,table
---

Continuing a [series of
posts](https://byorgey.github.io/blog/posts/2025/06/23/range-queries-classified.html)
on techniques for calculating *range queries*, today I will present
the [*sparse table* data structure](https://cp-algorithms.com/data_structures/sparse-table.html), for doing fast range queries on a
static sequence with an *idempotent* combining operation.

Motivation
----------

In my [previous
post](https://byorgey.github.io/blog/posts/2025/06/27/prefix-sums.html),
we saw that if we have a static sequence and a binary operation with a
*group* structure (*i.e.* every element has an inverse), we can
precompute a prefix sum table in $O(n)$ time, and then use it to answer
arbitrary range queries in $O(1)$ time.

What if we don't have inverses?  We can't use prefix sums, but can we
do something else that still allows us to answer range queries in
$O(1)$?  One thing we could always do would be to construct an $n
\times n$ table storing the answer to *every possible* range
query---that is, $Q[i,j]$ would store the value of the range $a_i
\diamond \dots \diamond a_j$. Then we could just look up the answer to
any range query in $O(1)$.  Naively computing the value of each
$Q[i,j]$ would take $O(n)$ time, for a total of $O(n^3)$ time to fill
in each of the entries in the table^[We only have to fill in $Q[i,j]$
where $i < j$, but this is still about $n^2/2$ entries.], though it's not
too hard to fill in the table in $O(n^2)$ total time, spending only
$O(1)$ to fill in each entry---I'll leave this to you as an exercise.

However, $O(n^2)$ is often too big.  Can we do better?  More
generally, we are looking for a particular *subset* of range queries
to precompute, such that the total number is asymptotically less than
$n^2$, but we can still compute the value of any arbitrary range query
by combining some (constant number of) precomputed ranges.  In the case
of a group structure, we were able to compute the values for only
prefix ranges of the form $1 \dots k$, then compute the value of an arbitrary
range using two prefixes, via subtraction.

A sparse table is exactly such a scheme for precomputing a subset of
ranges.^[In fact, I believe, but do not know for sure, that this is
where the name "sparse table" comes from---it is "sparse" in the sense
that it only stores a sparse subset of range values.] Rather than only
a linear number of ranges, as with prefix sums, we have to compute
$O(n \lg n)$ of them, but that's still way better than $O(n^2)$.  Note,
however, that a sparse table only works when the combining operation
is *idempotent*, that is, when $x \diamond x = x$ for all $x$. For
example, we can use a sparse table with combining operations such as
$\max$ or $\gcd$, but not with $+$ or $\times$.  Let's see how it works.

Sparse tables
-------------

The basic idea behind a sparse table is that we precompute a series of
"levels", where level $i$ stores values for ranges of length $2^i$.  So level
$0$ stores "ranges of length $1$"---that is, the elements of the
original sequence; level $1$ stores ranges of length $2$; level
$2$ stores ranges of length $4$; and so on. Formally, $T[i,j]$
stores the value of the range of length $2^i$ starting at index $j$.
That is,

$$T[i,j] = a_j \diamond \dots \diamond a_{j+2^i-1}.$$

We can see that $i$ only needs to go from $0$ up to $\lfloor \lg n
\rfloor$; above that and the stored ranges would be larger than
the entire sequence.  So this table has size $O(n \lg n)$.

Two important questions remain: how do we compute this table in the
first place? And once we have it, how do we use it to answer arbitrary
range queries in $O(1)$?

Computing the table is easy: each range on level $i$, of length $2^i$, is the
combination of two length-$2^{i-1}$ ranges from the previous level.  That is,

$$T[i,j] = T[i-1, j] \diamond T[i-1, j+2^{i-1}]$$

The zeroth level just consists of the elements of the original
sequence, and we can compute each subsequent level using values from
the previous level, so we can fill in the entire table in $O(n \lg n)$
time, doing just a single combining operation for each value in the table.

Once we have the table, we can compute the value of an arbitrary
range $[l,r]$ as follows:

- Compute the biggest power of two that fits within the range, that
  is, the largest $k$ such that $2^k \leq r - l + 1$.  We can compute
  this simply as $\lfloor \lg (r - l + 1) \rfloor$.
- Look up two range values of length $2^k$, one for the range which begins at $l$
  (that is, $T[k, l]$) and one for the range which ends at $r$ (that is, $T[k, r -
  2^k + 1]$).  These two ranges overlap; but because the combining
  operation is idempotent, combining the values of the ranges yields
  the value for our desired range $[l,r]$.

  This is why we require the combining operation to be idempotent:
  otherwise the values in the overlap would be overrepresented in the
  final, combined value.

Haskell code
------------

Let's write some Haskell code!  First, a little module for idempotent
semigroups.  Note that we couch everything in terms of *semigroups*,
not monoids, because we have no particular need of an identity
element; indeed, some of the most important examples like $\min$ and
$\max$ don't have an identity element.  The `IdempotentSemigroup`
class has no methods, since as compared to `Semigroup` it only adds a
law.  However, it's still helpful to signal the requirement.  You
might like to convince yourself that all the instances listed below
really are idempotent.

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
instance (IdempotentSemigroup a, IdempotentSemigroup b) => IdempotentSemigroup (a,b)
instance IdempotentSemigroup b => IdempotentSemigroup (a -> b)
```

Now, some code for sparse tables.  First, a few imports.

```haskell
{-# LANGUAGE TupleSections #-}

module SparseTable where

import Data.Array (Array, array, (!))
import Data.Bits (countLeadingZeros, finiteBitSize, (!<<.))
import IdempotentSemigroup
```

The sparse table data structure itself is just a 2D array over some
idempotent semigroup `m`.  Note that `UArray` would be more efficient,
but (1) that would make the code for building the sparse table more
annoying (more on this later), and (2) it would require a bunch of
tedious additional constraints on `m`.

```haskell
newtype SparseTable m = SparseTable (Array (Int, Int) m)
  deriving (Show)
```

We will frequently need to compute rounded-down base-two logarithms,
so we define a function for it.  A straightforward implementation
would be to repeatedly shift right by one bit and count the number of
shifts needed to reach zero; however, there is a better way, using
`Data.Bits.countLeadingZeros`.  It has a naive default implementation
which counts right bit shifts, but in most cases it compiles down to
much more efficient machine instructions.

```haskell
-- | Logarithm base 2, rounded down to the nearest integer.  Computed
--   efficiently using primitive bitwise instructions, when available.
lg :: Int -> Int
lg n = finiteBitSize n - 1 - countLeadingZeros n
```

Now let's write a function to construct a sparse table, given a
sequence of values. Notice how the sparse table array `st` is [defined
recursively](https://byorgey.github.io/blog/posts/2023/06/02/dynamic-programming-in-haskell-lazy-immutable-arrays.html).
This works because the `Array` type is lazy in the stored values, with
the added benefit that only the array values we end up actually
needing will be computed.  However, this comes with a decent amount of
overhead.  If we wanted to use an unboxed array instead, we wouldn't
be able to use
the recursive definition trick; instead, we would have to [use an
`STUArray`](https://byorgey.github.io/blog/posts/2021/11/17/competitive-programming-in-haskell-bfs-part-4-implementation-via-stuarray.html)
and fill in the values in a specific order.  The code for this would
be longer and much more tedious, but could be faster if we end up
needing all the values in the array anyway.

```haskell
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
      zip ((0,) <$> [0 ..]) ms
        ++ [ ((i, j), st ! (i - 1, j) <> st ! (i - 1, j + 1 !<<. (i - 1)))
           | i <- [1 .. lgn]
           , j <- [0 .. n - 1 !<<. i]
           ]
```

Finally, we can write a function to answer range queries.

```haskell
-- | \$O(1)$. @range st l r@ computes the range query which is the
--   @sconcat@ of all the elements from index @l@ to @r@ (inclusive).
range :: IdempotentSemigroup m => SparseTable m -> Int -> Int -> m
range (SparseTable st) l r = st ! (k, l) <> st ! (k, r - (1 !<<. k) + 1)
 where
  k = lg (r - l + 1)

```

Applications
------------

Most commonly, we can use a sparse table to find the minimum or
maximum values on a range, $\min$ and $\max$ being the quintessential
idempotent operations.  For example, this plays a key role in a
solution to the (quite tricky) problem
[Ograda](https://open.kattis.com/problems/ograda).^[At first it
seemed like that problem should be solvable with some kind of [sliding
window](https://byorgey.github.io/blog/posts/2024/11/27/stacks-queues.html) approach, but I couldn't figure out how to make it work!]

What if we want to find the *index of* the minimum or maximum value in
a given range (see, for example, [Worst Weather](https://open.kattis.com/problems/worstweather))?  We can easily accomplish this using the semigroup `Min
(Arg m i)` (or `Max (Arg m i)`), where `m` is the type of the values and `i` is
the index type. `Arg`, from `Data.Semigroup`, is just a pair which uses only the first value
for its `Eq` and `Ord` instances, and carries along the second value
(which is also exposed via `Functor`, `Foldable`, and `Traversable`
instances). In the example below, we can see that the call to `range
st 0 3` returns both the max value on the range (`4`) and its index
(`2`) which got carried along for the ride:

```
λ> :m +Data.Semigroup
λ> st = fromList (map Max (zipWith Arg [2, 3, 4, 2, 7, 4, 9] [0..]))
λ> range st 0 3
Max {getMax = Arg 4 2}
```

Finally, I will mention that being able to compute range minimum
queries is one way to compute lowest common ancestors for a (static,
rooted) tree.  First, walk the tree via a depth-first search and
record the depth of each node encountered in sequence, a so-called
[Euler tour](https://en.wikipedia.org/wiki/Euler_tour_technique) (note
that you must record *every* visit to a node---before visiting any of
its children, in between each child, and after visiting all the
children).  Now the minimum depth recorded between visits to any two
nodes will correspond to their lowest common ancestor.

Here are a few problems that involve computing least common ancestors
in a tree, though note there are also other techniques for computing
LCAs (such as binary jumping) which I plan to write about eventually.

* [Tourists](https://open.kattis.com/problems/tourists)
* [Stogovi](https://open.kattis.com/problems/stogovi)
* [Win Diesel](https://open.kattis.com/problems/windiesel)
