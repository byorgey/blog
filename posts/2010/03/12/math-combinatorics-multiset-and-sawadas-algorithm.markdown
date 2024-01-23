---
title: Math.Combinatorics.Multiset and Sawada's algorithm
published: 2010-03-12T15:20:35Z
categories: haskell,links,math
tags: algorithm,cycles,multiset,necklaces,Sawada
---

I've uploaded a <a href="http://hackage.haskell.org/package/multiset%2Dcomb">new version of my Math.Combinatorics.Multiset library</a> (see the <a href="http://byorgey.wordpress.com/2010/02/20/math-combinatorics-multiset/">previous announcement here</a>).  I've added a few more fairly simple algorithms (splitting a multiset into two pieces in all possible ways; finding all size-k submultisets of a multiset), and one which is decidedly NOT simple.  I wanted to generate all distinct <i>cyclic arrangements</i> of the elements from a multiset -- in other words, sequences which are distinct up to cyclic rotations.  For example, the multiset $latex \{1,1,2,2,3\}$ has six distinct cyclic arrangements:

$latex \langle 11223\rangle, \langle 11232\rangle, \langle 11322\rangle, \langle 12123\rangle, \langle 12132\rangle, \langle 12213\rangle$

How to generate all such cyclic arrangements (also called "necklaces"), given a multiset?  I thought about it for a whole day and decided that it was rather difficult!  (If you don't believe me, try coding it yourself.)  A bit of searching confirmed my suspicion but turned up a nice <a href="http://www.cis.uoguelph.ca/~sawada/papers/alph.pdf">2003 paper by Joe Sawada</a> with a fast algorithm (which I still don't quite understand -- it depends on some non-trivial properties of necklaces!). So, I implemented it, as <code>Math.Combinatorics.Multiset.cycles</code>.

