---
katex: true
title: 'Math.Combinatorics.Multiset'
published: 2010-02-20T23:03:42Z
categories: haskell,projects
tags: algorithm,combinatorial,multiset,partitions,permutations
---

Just a quick note to say that I've just uploaded a new package, <a href="http://hackage.haskell.org/package/multiset%2Dcomb">multiset-comb</a>, to Hackage. It primarily has functions for generating all the distinct permutations and partitions of a multiset (a set where elements are allowed to occur multiple times). <i>Efficiently</i> generating distinct permutations and partitions of a multiset is nontrivial (generating all partitions and permutations as if all the elements were distinct, and then discarding duplicate results, is hopelessly inefficient). The permutations code is new; the partitions code is a slight generalization of the code from my article in <a href="https://wiki.haskell.org/wikiupload/d/dd/TMR-Issue8.pdf">Issue 8 of the Monad.Reader</a>. I put this code together primarily because I need it for my combinatorial species library (look for a new release of that soon!), but it's fairly independent, and I thought others might find it useful, so I'm releasing it as a separate library.

