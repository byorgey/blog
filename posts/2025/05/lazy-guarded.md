---
title: 'Lazy evaluation and guarded recursion'
tags: Haskell,lazy,evaluation,tail,guarded,recursion
katex: true
---

XXX introduction

Tail recursion
==============

*Tail recursion* is recursion where the result of a recursive call is
directly used as the result of the whole function, that is, when there
is nothing left to do with the result of the recursive call except
return it.

For example, consider the following two functions:

```haskell
sum1 :: [Integer] -> Integer
sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

sum2 :: Integer -> [Integer] -> Integer
sum2 s [] = s
sum2 s (x : xs) = sum2 (s + x) xs
```

`sum2` is tail recursive: the result of the recursive call
`sum2 (s + x) xs` is directly used as the result of `sum2 s (x : xs)`.  On the
other hand, `sum1` is not tail recursive: the result of the recursive
call `sum1 xs` is not directly used as the result, but is embedded in
a larger expression.  In particular, the value of `x` must still be
added to the result of `sum1 xs`.

There are no value judgments here---whether tail-recursive or
non-tail-recursive is better (or whether it even matters) depends a
lot on context.

Converting to tail-recursive code
---------------------------------

If we do want tail recursion for some reason, though, how can we
achieve it?  `sum1` and `sum2` illustrate on technique that works
sometimes: use an extra parameter to accumulate the answer along the
way, so that in the base case we can simply return the final answer,
rather than having to construct the answer on the way back up the
recursion stack.

However, this technique is not always applicable. XXX more examples
A more general technique is *continuation-passing style* (CPS), where
we pass along an extra *continuation* parameter, which is a function
that says what to do with the result.  For example:

```haskell
sumCPS :: (Integer -> r) -> [Integer] -> r
sumCPS k [] = k 0
sumCPS k (x : xs) = sumCPS (k . (x+)) xs
```

This rabbit hole goes very deep, and this blog post is not intended as
a CPS tutorial; you can read more about it XXX

Evaluating function calls
=========================

Any programming language with functions must answer the question: how
are function calls evaluated at runtime?  There are two main approaches:
stack frames and rewriting.

Function calls via stack frames
-------------------------------



Function calls via rewriting
----------------------------
