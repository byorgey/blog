---
title: New ICFP functional pearl on subtracting bijections
published: 2018-06-23T21:47:58Z
categories: combinatorics,haskell,writing
tags: bijections,difference,ICFP,paper,pearl,subtraction
---

<p>Kenny Foner and I have <a href="http://ozark.hendrix.edu/~yorgey/pub/GCBP-author-version.pdf">a paper</a> accepted to ICFP on <em>subtracting bijections</em>. Here’s the basic problem: suppose you have a bijection $latex h$ between two sum types, $latex h : A + B \leftrightarrow A' +B'$, and another bijection $latex g : B \leftrightarrow B'$. Of course $latex A$ and $latex A'$ must have the same size, but can you construct an actual bijection $latex f$ between $latex A$ and $latex A'$?</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2018/06/598773235eacd729.png" /></p>
</div>
<p>This problem and its solution has been well-studied in the context of combinatorics; we were wondering what additional insight could be gained from a higher-level functional approach. You can <a href="http://ozark.hendrix.edu/~yorgey/pub/GCBP-author-version.pdf">get a preprint here</a>.</p>

