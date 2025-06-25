---
title: 'Competitive programming in Haskell: prefix sums'
categories: Haskell,competitive programming
katex: true
tags: monoid,range,query,prefix,sum
published: 2025-06-25T00:00:00Z
---

In a [previous blog
post](https://byorgey.github.io/blog/posts/2025/06/23/range-queries-classified.html)
I categorized a number of different techniques for calculating *range queries*.
Today, I will discuss one of those techniques which is simple but frequently
useful.

Precomputing prefix sums
------------------------

Suppose we have a static sequence of values $a_1, a_2, a_3, \dots,
a_n$ drawn from some
[group](https://en.wikipedia.org/wiki/Group_(mathematics))^[That is,
there is an associative binary operation, and
every element has an inverse.], and want
to be able to compute the total value (according to the group
operation) of any contiguous subrange.  That is, given a range
$[i,j]$, we want to compute $a_i \diamond a_{i+1} \diamond \dots
\diamond a_j$ (where $\diamond$ is the group operation).  For example,
we might have a sequence of integers and want to compute the sum, or
perhaps the bitwise xor (but not the maximum) of all the values in any particular
subrange.

Of course, we could simply compute $a_i \diamond \dots \diamond a_j$
directly, but that takes $O(n)$ time.  With some simple preprocessing,
it's possible to compute the value of any range in constant time.

The key idea is to precompute an array $P$ of *prefix sums*, so $P_i =
a_1 \diamond \dots \diamond a_i$.  This can be computed in linear time
via a *scan*; for example:

```haskell
import Data.Array
import Data.List (scanl')

prefix :: Monoid a => [a] -> Array Int a
prefix a = listArray (0, length a) $ scanl' (<>) mempty a
```
^[{-} Actually, I would typically use an *unboxed* array, which is
faster but slightly more limited in its uses: import
`Data.Array.Unboxed`, use `UArray` instead of `Array`, and add an
`IArray UArray a` constraint.]

Note that we set $P_0 = 0$ (or whatever the identity element is for
the group); this is why I had the sequence of values indexed starting
from $1$, so $P_0$ corresponds to the empty sum, $P_1 = a_1$, $P_2 =
a_1 \diamond a_2$, and so on.

Now, for the value of the range $[i,j]$, just compute $P_j \diamond
P_{i-1}^{-1}$---that is, we start with a prefix that ends at the right place, then
cancel or "subtract" the prefix that ends right before the range we
want.  For example, to find the sum of the integers $a_5 + \dots +
a_{10}$, we can compute $P_{10} - P_4$.

```haskell
range :: Group a => Array Int a -> Int -> Int -> a
range p i j = p!j <> inv (p!(i-1))
```

That's why this only works for groups but not for general monoids:
only in a group can we *cancel* unwanted values.  So, for example,
this works for finding the sum of any range, but not the maximum.

Practice problems
-----------------

Want to practice?  Here are a few problems that can be solved using
techniques discussed in this post:

* [Determining Nucleotide Assortments](https://open.kattis.com/problems/nucleotides)
* [Einvígi](https://open.kattis.com/problems/einvigi)
* [Srednji](https://open.kattis.com/problems/srednji)
* [Veggja Kalli](https://open.kattis.com/problems/veggjakalli)

It is possible to generalize this scheme to 2D---that is, to compute
the value of any *subrectangle* of a *2D grid* of values from some
group in only $O(1)$ time.  I will leave you the fun of figuring out
the details.

* [Prozor](https://open.kattis.com/problems/prozor)
* [Rust](https://open.kattis.com/problems/rust)

If you're looking for an extra challenge, here are a few harder
problems which use techniques from this post as an important
component, but require some additional nontrivial ingredients:

* [Killing Chaos](https://open.kattis.com/problems/killingchaos)
* [Ozljeda](https://open.kattis.com/problems/ozljeda)
* [Vudu](https://open.kattis.com/problems/vudu)
