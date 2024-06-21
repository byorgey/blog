---
title: 'Competitive Programming in Haskell: sieving'
categories: competitive programming,haskell
tags: challenge,Kattis,number theory,sieve,mutable array
katex: true
---

In [a previous
post](https://byorgey.github.io/blog/posts/2024/05/28/competitive-programming-in-haskell-two-problems.html)
I challenged you to solve [Product
Divisors](https://open.kattis.com/problems/productdivisors).  In this
problem, we are given a sequence of positive integers $a_1, \dots,
a_n$, and we are asked to compute the total number of divisors of
their product.  For example, if we are given the numbers $4, 2, 3$,
then the answer should be $8$, since $4 \times 2 \times 3 = 24$ has
the $8$ distinct divisors $1, 2, 3, 4, 6, 8, 12, 24$.

## Counting divisors

In general, if $a$ has the prime factorization $a = p_1^{\alpha_1} p_2^{\alpha_2}
\cdots p_k^{\alpha_k}$ (where the $p_i$ are all distinct primes), then
the number of divisors of $a$ is

$$(\alpha_1 + 1)(\alpha_2 + 1) \cdots (\alpha_k + 1),$$

since we can independently choose how many powers of each prime to
include.  There are $\alpha_i + 1$ choices for $p_i$ since we can
choose anything from $p_i^0$ up to $p_i^{\alpha_i}$, inclusive.

So at a fundamental level, the solution is clear: factor each $a_i$,
count up the number of copies of each prime in their product, then do
something like `product . map succ`.  We are also told the answer
should be given mod $10^9 + 7$, so we can use a [`newtype` with a
custom `Num` instance](https://byorgey.github.io/blog/posts/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1.html):

```haskell
p :: Integer
p = 10^9 + 7

newtype M = M { unM :: Integer } deriving (Eq, Ord)
instance Show M where show = show . unM
instance Num M where
  fromInteger = M . (`mod` p) . fromInteger
  M x + M y = M ((x + y) `mod` p)
  M x - M y = M ((x - y) `mod` p)
  M x * M y = M ((x * y) `mod` p)
```

## A naïve solution (TLE)

Of course, I would not be writing about this problem if it were that
easy!  If we try implementing the above solution idea in a
straightforward way---for example, if we [take the code from this blog
post](https://byorgey.wordpress.com/2020/02/07/competitive-programming-in-haskell-primes-and-factoring/)
and then do something like `product . map succ . M.elems . M.unionsWith (+) . map factor`, we get the dreaded Time Limit Exceeded.

Why doesn't this work?  I haven't mentioned how many integers might be
in the input: in fact, we might be given as many as one million ($10^6$)!  We need
to be able to factor each number very quickly if we're going to finish
within the time limit (1 second).  Factoring each number from scratch
by trial division is simply too slow.

## Factoring via sieve

While much more sophisticated methods are needed to factor a *single*
number more quickly than trial division, there is a standard technique
we can use to speed things up when we need to factor *many* numbers.
We can use a *sieve* to precompute a lookup table, which we can then
use to factor numbers very quickly.

In particular, we will compute a table $\mathit{smallest}$ such that
$\mathit{smallest}[i]$ will store the *smallest prime factor* of $i$.
Given this table, to factor a positive integer $i$, we simply look up
$\mathit{smallest}[i] = p$, add it to the prime factorization, then
recurse on $i/p$.  The base case is when $i = 1$, in which case we
return an empty factorization.

How do we compute $\mathit{smallest}$?  The basic idea is to create an
array of size $n$, initializing it with $\mathit{smallest}[k] =
k$. For each $k$ from $2$ up to $n$,^[{-} We could optimize this even
further via the [approach in this blog
post](https://codeforces.com/blog/entry/54090), which takes $O(n)$
rather than $O(n \lg n)$ time, but it would complicate our Haskell
quite a bit and it's not needed for solving this problem.] if
$\mathit{smallest}[k]$ is still equal to $k$, then $k$ must be prime;
iterate through multiples of $k$ (starting with $k^2$, since any
smaller multiple of $k$ is already divisible by a smaller prime) and
set each $\mathit{smallest}[ki]$ to the minimum of $k$ and whatever
value it had before.

## Sieving in Haskell

This is one of those cases where for efficiency's sake, we actually
want to use an honest-to-goodness mutable array.  Immutable arrays are
not a good fit for sieving, and using something like a `Map` would
introduce a lot of overhead that we would rather avoid.  However, we
only need the table to be mutable while we are computing it; after
that, it should just be an immutable lookup table.  This is great fit
for an `STUArray`:^[{-} Note that as of this writing, the version of the
`array` library installed in the Kattis environment does not have
`modifyArray'`, so we actually have to do `readArray` followed by
`writeArray`.]

```haskell
maxN = 1000000

smallest :: UArray Int Int
smallest = runSTUArray $ do
  a <- newListArray (2,maxN) [2 ..]
  forM_ [2 .. maxN] $ \k -> do
    k' <- readArray a k
    when (k == k') $ do
      forM_ [k*k, k*(k+1) .. maxN] $ \n ->
        modifyArray' a n (min k)
  return a
```

Haskell, the [world's finest imperative programming language](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf)!

## Combining factorizations

So now we're almost done, right?  We can write a new `factor` function
that works by repeatedly looking up the smallest prime factor:

```haskell
factor :: Int -> Map Int Int
factor = \case
  1 -> M.empty
  n -> M.insertWith (+) p 1 (factor (n `div` p))
   where
    p = smallest!n
```

And now

```haskell
solve :: Int -> 
```
