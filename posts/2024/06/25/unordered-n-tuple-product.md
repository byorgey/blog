---
title: 'Products with unordered n-tuples'
categories: haskell
tags: type-level programming,Haskell,product,unordered,tuples
katex: true
---

Recently, Dani Rybe wrote [this really cool blog
post](https://danryba253.github.io/danirybeblog/posts/unordered-n-tuples/)
(in turn based on [this old post by Samuel
Gélineau](https://gelisam.blogspot.com/2013/07/the-commutative-monad.html))
about encoding truly *unordered* n-tuples in Haskell.  This is
something I thought about a long time ago in my work on
combinatorial species, but I never came up with a way to represent
them.  Samuel and Dani's solution is wonderful and clever and totally
impractical, and I love it.

I won't go into more detail than that; I'll let you go read it if
you're interested.  This blog post exists solely to respond to Dani's
statement towards the end of her post:

> I’m not sure how to, for example, write a function that multiplies
> the inputs.

Challenge accepted!

```haskell
primes :: [Int]
primes = 2 : sieve primes [3 ..]
 where
  sieve (p : ps) xs =
    let (h, t) = span (< p * p) xs
     in h ++ sieve ps (filter ((/= 0) . (`mod` p)) t)

mul :: [Int] -> Int
mul = unfuck mulU
 where
  mulU :: U n Int -> Int
  mulU = ufold 1 id (< 0) \(US neg nonNeg) ->
    mulNonNeg nonNeg * mulPos primes (abs <$> neg) * (-1) ^ ulen neg

  mulNonNeg :: U n Int -> Int
  mulNonNeg = ufold 1 id (== 0) \(US zero pos) ->
    if ulen zero > 0 then 0 else mulPos primes pos

  mulPos :: [Int] -> U n Int -> Int
  mulPos ps = ufold 1 id (== 1) \(US _ pos) -> mulGTOne ps pos

  mulGTOne :: [Int] -> U n Int -> Int
  mulGTOne (p : ps) = ufold 1 id ((== 0) . (`mod` p)) \(US divP nondivP) ->
    mulPos (p : ps) ((`div` p) <$> divP) * (p ^ ulen divP) * mulGTOne ps nondivP
```

Since every integer has a unique prime factorization, at each step we
split the remaining numbers into those divisible by $p$ and those not
divisible by $p$.  For the ones that are, we divide out $p$ from all
of them, multiply by the appropriate power of $p$, and recurse on
what's left; for those that are not, we move on to trying the next
prime.

Dani also speculates about `ubind :: U n (U m a) -> U (n :*: m) a`.  I
believe in my heart this should be possible to implement, but after
playing with it a bit, I concluded it would require an astounding feat
of type-fu.

PS I'm working on getting comments set up here on my new
blog... hopefully coming soon!
