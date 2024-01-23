---
title: The Poisson distribution and Stirling numbers
published: 2008-09-16T12:11:19Z
categories: combinatorics,grad school,learning,math
tags: Bell numbers,moments,Poisson,Stirling numbers
---

While working on an assignment for my machine learning class, I rediscovered the fact that if X is a random variable from a <a href="http://mathworld.wolfram.com/PoissonDistribution.html">Poisson distribution</a> with parameter $latex \lambda$, then

$latex \displaystyle E[X^n] = \sum_{k=1}^n S(n,k) \lambda^k,$

where $latex S(n,k)$ denotes a <a href="http://mathworld.wolfram.com/StirlingNumberoftheSecondKind.html">Stirling number of the second kind</a>. (I actually prefer Knuth's curly bracket notation, but I can't seem to get it to work on this blog.)  In particular, if $latex \lambda = 1$, then $latex E[X^n]$ is the nth Bell number $latex B_n$, the number of ways of partitioning a set of size n into subsets!

As it turned out, this didn't help me at all with my assignment, I just thought it was nifty.

