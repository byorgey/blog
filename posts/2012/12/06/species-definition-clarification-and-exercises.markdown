---
katex: true
title: 'Species definition clarification and exercises'
published: 2012-12-06T16:44:33Z
categories: math,species
tags: exercises,finite,functor,size
---

<!-- -*- compile-command: "BlogLiteratelyD 04-defn2.markdown &gt; 04-defn2.html" -*- -->
<p>[<em>This is the fourth in a series of posts about combinatorial species. Previous posts: <a href="https://byorgey.github.io/blog/posts/2012/10/17/and-now-back-to-your-regularly-scheduled-combinatorial-species.html">And now, back to your regularly scheduled combinatorial species</a>; <a href="https://byorgey.github.io/blog/posts/2012/10/27/decomposing-data-structures.html">Decomposing data structures</a>; <a href="https://byorgey.github.io/blog/posts/2012/11/20/combinatorial-species-definition.html">Combinatorial species definition</a>.</em>]</p>
<p>In <a href="https://byorgey.github.io/blog/posts/2012/11/20/combinatorial-species-definition.html">my previous post</a> I neglected to mention something quite crucial, namely, that (at least for now) we are only talking about <em>finite</em> sets of labels and <em>finite</em> sets of structures. That is, we do not consider structures with infinitely many labels, and given a particular finite set of labels, there must be only a finite number of structures with those labels. The category $\mathbb{B}$ is the category of <em>finite</em> sets and bijections, not of all sets as I said previously.</p>
<p>Of course, in practice we <em>do</em> want to think about infinite sets of structures and labels, especially in relation to a non-strict language like Haskell! But the theory was invented by mathematicians interested in <em>counting things</em>. I do intend to explore ways to extend the theory to encompass infinite structures, but for now we’ll stick to the finite.</p>
<p>Before moving on to talk about the algebraic approach to species, I also want to point out a few simple implications of the <a href="https://byorgey.github.io/blog/posts/2012/11/20/combinatorial-species-definition.html">formal definition</a>. Instead of spelling them out in detail, I will pose them as exercises: you can either take them on faith, or try working through the exercises to deepen your understanding.</p>
<ol style="list-style-type:decimal;">
<li>Let $[n]$ denote the set $\{0, \dots, n-1\}$<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup>, and write $F[n]$ as an abbreviation for $F[[n]]$, <em>i.e.</em> the application of the species $F$ to the label set $[n]$. Show that given $F[n]$ we can determine $F[U]$ for any $U$ with $|U| = n$.</li>
</ol>
<p>(This shows that “size is all that matters”: in some sense species are really indexed not by sets of labels but by <em>size</em>.)</p>
<ol start="2" style="list-style-type:decimal;">
<li>[BLL<sup><a href="#fn2" class="footnoteRef" id="fnref2">2</a></sup> Exercise 1.1.2] Show that we get an equivalent definition if we take species to be functors from $\mathbb{B}$ to $\mathbf{FinSet}$ (the category of finite sets and (total) functions) instead of endofunctors on $\mathbb{B}$.</li>
</ol>
<p>(Apparently one can generalize the notion of species by replacing $\mathbf{FinSet}$ with other categories, but at present I am not sure of the implications.)</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>The species literature uses $\{1, \dots, n\}$, but (as every good computer scientist knows) counting ought to begin at zero.<a href="#fnref1">↩</a></p></li>
<li id="fn2"><p>I will use “BLL” to refer to Bergeron, Labelle, and Leroux, <em>Combinatorial Species and Tree-Like Structures</em> (see the references at the end of <a href="https://byorgey.github.io/blog/posts/2012/11/20/combinatorial-species-definition.html">the previous post</a>  for the full citation).<a href="#fnref2">↩</a></p></li>
</ol>
</div>

