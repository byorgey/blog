---
katex: true
title: 'And now, back to your regularly scheduled combinatorial species'
published: 2012-10-18T01:43:42Z
categories: combinatorics,math,writing
tags: combinatorial species,research
---

<p>I’ve already mentioned this to people here and there, but haven’t yet announced it publically, so here it is: <a href="http://www.cis.upenn.edu/~sweirich/">Stephanie Weirich</a> and I have been awarded a grant from the NSF to study the intersection of <em>combinatorial species</em> and (functional) programming, and so I’ll be returning the topic for my dissertation.</p>
<p>I’ve always found blogging to be an excellent way to organize my thoughts, and it often prompts great feedback and insights from readers which fuel further exploration. So as one aspect of my research, I plan to write a series of blog posts explaining the theory of combinatorial species and its relationship to algebraic data types. I’ll start with the very basics and (hopefully) progress to some deeper results, pulling together references to related things along the way.</p>
<p>I’ve written about species on this blog before (<a href="https://byorgey.github.io/blog/posts/2009/07/24/introducing-math-combinatorics-species.html">here</a>, <a href="https://byorgey.github.io/blog/posts/2009/07/30/primitive-species-and-species-operations.html">here</a>, <a href="https://byorgey.github.io/blog/posts/2009/07/31/primitive-species-and-species-operations-part-ii.html">here</a>, <a href="https://byorgey.github.io/blog/posts/2009/08/05/species-operations-differentiation.html">here</a>, and <a href="https://byorgey.github.io/blog/posts/2010/11/24/species-subtraction-made-simple.html">here</a>), and I <a href="http://www.cis.upenn.edu/~byorgey/pub/species-pearl.pdf">published a paper in the 2010 Haskell Symposium</a> on the topic, so I’ll certainly end up duplicating some of that content. But it’s worth starting over from the beginning, for several reasons:</p>
<ul>
<li>I want as many people as possible to be able to follow along, without having to tell them “first go back and read these blog posts from 2009”.</li>
<li>I’m not completely happy with the way I presented some of that material in the past; in the intervening years I feel I’ve had some better insights into how everything fits together.</li>
<li>Those previous posts—and my Haskell Symposium paper—conflated explaining species with explaining my Haskell library for computing with species,<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup> which I now think is not all that helpful, because it glosses over too many subtle issues with the relationship of species to algebraic data types.</li>
</ul>
<p>So, in my next post, I’ll begin by <em>defining</em> species—but with some extra context and insight that I hope you’ll find enlightening, even if you already know the definition.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>It’s on <a href="http://hackage.haskell.org/package/species">Hackage here</a>,  but I haven’t touched it in a long time and it doesn’t build with  recent versions of GHC. I plan to fix that soon.<a href="#fnref1">↩</a></p></li>
</ol>
</div>

