---
katex: true
title: 'Boltzmann sampling for generic Arbitrary instances'
published: 2016-03-23T17:58:01Z
categories: combinatorics,haskell,math,species
tags: Arbitrary,Boltzmann,generation,QuickCheck,random,sampling
---

<em>Update, 7/17/2017: this now exists; see <a href="https://byorgey.wordpress.com/2016/09/20/the-generic-random-library-part-1-simple-generic-arbitrary-instances/">https://byorgey.wordpress.com/2016/09/20/the-generic-random-library-part-1-simple-generic-arbitrary-instances/</a> .</em>

<strong>tl;dr</strong>: <em>I know how to generate random instances of data types in a generic way, and even have some old code that already does all the hard work, but won’t have time to polish and package it until this summer. If you’re interested in helping, let me know!</em>

This morning Kenny Foner pointed out to me <a href="https://twitter.com/GabrielG439/status/712482663395753984">this tweet by Gabriel Gonzales</a>, asking why there isn’t a default <a href="http://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Arbitrary.html#t:Arbitrary"><code>Arbitrary</code></a> instance for types implementing <code>Generic</code>. It reminded me that I’ve been meaning for a while now (years, in fact!) to get around to packaging up some code that does this.

As several pointed out on Twitter, this <em>seems</em> obvious, but it isn’t. It’s easy to write a generic <code>Arbitrary</code> instance, but hard to write one that generates a good distribution of values. The basic idea is clear: randomly pick a constructor, and then recursively generate random subtrees. The problem is that this is very likely to either blow up and generate gigantic (even infinite) trees, or to generate almost all tiny trees, <em>or both</em>. I wrote <a href="https://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/">a post about this three years ago</a> which illustrates the problem. It also explains half of the solution: generate random trees with a target size in mind, and throw out any which are not within some epsilon of the target size (crucially, stopping the generation early as soon as the tree being generated gets too big).

However, I never got around to explaining the other half of the solution: it’s crucially important to use the right probabilities when picking a constructor. With the wrong probabilities, you will spend too much time generating trees that are either too small or too big. The surprising thing is that with exactly the <em>right</em> probabilities, you can expect to wait only $O(n)$ time before generating a tree of size (approximately<a id="fnref1" class="footnoteRef" href="#fn1"><sup>1</sup></a>) $n$.<a id="fnref2" class="footnoteRef" href="#fn2"><sup>2</sup></a>

So, how does one pick the right probabilities? Essentially, you turn the generic description of your data type into a mutually recursive system of generating functions, and (numerically) find their radii of convergence, when thought of as functions in the complex plane. Using these values it is straightforward to compute the right probabilities to use. For the intrepid, this is explained in Duchon <em>et. al</em><a id="fnref3" class="footnoteRef" href="#fn3"><sup>3</sup></a>.

I have some old Haskell code from Alexis Darrasse which already does a bunch of the work. It would have to be updated a bit to work with modern libraries and with <code>GHC.Generics</code>, and packaged up to go on Hackage. I won’t really have time to work on this until the summer—but if anyone else is interested in working on this, let me know! I’d be happy to send you the code and provide some guidance in figuring it out.
<div id="refs" class="references"></div>
<div class="footnotes">

<hr />

<ol>
	<li id="fn1">The constant factor depends on how approximate you are willing to be.<a href="#fnref1">↩</a></li>
	<li id="fn2">I wanted to put an exclamation point at the end of that sentence, because this is really surprising. But it looked like $n$ factorial. So, here is the exclamation point: !<a href="#fnref2">↩</a></li>
	<li id="fn3">Duchon, Philippe, <em>et al.</em> “Boltzmann samplers for the random generation of combinatorial structures.” Combinatorics Probability and Computing 13.4-5 (2004): 577-625.<a href="#fnref3">↩</a></li>
</ol>
</div>

