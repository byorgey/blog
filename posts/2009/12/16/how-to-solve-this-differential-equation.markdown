---
title: 'How to solve this differential equation?'
published: 2009-12-16T14:01:38Z
categories: combinatorics,math
tags: Bell numbers,differential,equation,generating function,partitions
---

How would you solve the differential equation

$B'(x) = e^x B(x)$

with the initial condition $B(0) = 1$?  I know what the answer is supposed to be, but I don't know how to directly solve it.

In case you're wondering, $B(x)$ is the <a href="http://en.wikipedia.org/wiki/Generating_function">exponential generating function</a> for the <a href="http://mathworld.wolfram.com/BellNumber.html">Bell numbers</a>, which count set partitions.  The differential equation in question arises from noting that

$\displaystyle B_{n+1} = \sum_{k=0}^n \binom n k B_{n-k}$

(to make a partition of $\{1, \dots, n+1\}$, you can put anywhere from $k = 0$ to $n$ elements in the same set with $n+1$; there are $\binom n k$ ways to choose the $k$ elements to include, and $B_{n-k}$ ways to partition the rest).

