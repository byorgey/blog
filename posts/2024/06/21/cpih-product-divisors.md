---
title: 'Competitive Programming in Haskell: sieving with mutable arrays'
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
something like `map (+1) >>> product`.  We are also told the answer
should be given mod $10^9 + 7$, so we can use a^[{-} Using `Int`
instead of `Integer` here is OK as long as we are sure to be running
on a 64-bit system; multiplying two `Int` values up to $10^9 + 7$
yields a result that still fits within a 64-bit signed `Int`.
Otherwise (*e.g.* [on Codeforces](https://byorgey.github.io/blog/posts/2021/09/21/competitive-programming-in-haskell-codeforces-educational-round-114.html)) we would have to use `Integer`.] [`newtype` with a
custom `Num` instance](https://byorgey.github.io/blog/posts/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1.html):

```haskell
p :: Int
p = 10^9 + 7

newtype M = M { unM :: Int } deriving (Eq, Ord)
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
straightforward way---for example, if we [take the simple factoring code from this blog
post](https://byorgey.wordpress.com/2020/02/07/competitive-programming-in-haskell-primes-and-factoring/)
and then do something like `map factor >>> M.unionsWith (+) >>> M.elems >>> map (+1) >>> product`, we get the dreaded Time Limit Exceeded.

Why doesn't this work?  I haven't mentioned how many integers might be
in the input: in fact, we might be given as many as one million ($10^6$)!  We need
to be able to factor each number very quickly if we're going to finish
within the one second time limit.  Factoring each number from scratch
by trial division is simply too slow.

## Factoring via sieve

While [more sophisticated methods](https://cp-algorithms.com/algebra/factorization.html) are needed to factor a *single*
number more quickly than trial division, there is a standard technique
we can use to speed things up when we need to factor *many* numbers.
We can use a *sieve* to precompute a lookup table, which we can then
use to factor numbers very quickly.

In particular, we will compute a table $\mathit{smallest}$ such that
$\mathit{smallest}[i]$ will store the *smallest prime factor* of $i$.
Given this table, to factor a positive integer $i$, we simply look up
$\mathit{smallest}[i] = p$, add it to the prime factorization, then
recurse on $i/p$; the base case is when $i = 1$.

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
that, it should just be an immutable lookup table.  This is a great fit
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

We can now write a new `factor` function that works by repeatedly
looking up the smallest prime factor:

```haskell
factor :: Int -> Map Int Int
factor = \case
  1 -> M.empty
  n -> M.insertWith (+) p 1 (factor (n `div` p))
   where
    p = smallest!n
```

And now we can just do `map factor >>> M.unionsWith (+) >>> M.elems >>> map (+1) >>> product`
as before, but since our `factor` is so much faster this time, it
should...

What's that?  Still TLE?  Sigh.

## Counting primes via a (second) mutable array

Unfortunately, creating a bunch of `Map` values and then doing
`unionsWith` one million times still introduces way too much overhead.
For many problems working with `Map` (which is impressively fast) is
good enough, but not in this case.  Instead of returning a `Map` from
each call to `factor` and then later combining them, we can write a version of
`factor` that directly increments counters for each prime in a
mutable array:

```haskell
factor :: STUArray s Int Int -> Int -> ST s ()
factor counts n = go n
  where
    go 1 = return ()
    go n = do
      let p = smallest!n
      modifyArray' counts p (+1)
      go (n `div` p)
```

Then we have the following top-level solution, which is finally fast
enough:

```haskell
main :: IO ()
main = C.interact $ runScanner (numberOf int) >>> solve >>> showB

solve :: [Int] -> M
solve = counts >>> elems >>> map ((+1) >>> M) >>> product

counts :: [Int] -> UArray Int Int
counts ns = runSTUArray $ do
  cs <- newArray (2,maxN) 0
  forM_ ns (factor cs)
  return cs
```

This solution runs in just over 0.4s for me.  Considering that this is
only about 4x slower than the fastest solution (0.09s, in C++), I'm
pretty happy with it!  We did have to sacrifice a bit of elegance for
speed, especially with the `factor` and `counts` functions instead of
`M.unionsWith`, but in the end it's not too bad.

I thought we might be able to make this even faster by using a strict
fold over the `counts` array instead of converting to a list with
`elems` and then doing a `map` and a `product`, but (1) there is no
generic fold operation on `UArray`, and (2) I trust that GHC is
already doing a pretty good job optimizing this via list fusion.

## Next time

Next time I'll write about my solution to the [other challenge
problem](https://byorgey.github.io/blog/posts/2024/05/28/competitive-programming-in-haskell-two-problems.html),
[Factor-Full Tree](https://open.kattis.com/problems/factorfulltree).
Until then, give it a try!
