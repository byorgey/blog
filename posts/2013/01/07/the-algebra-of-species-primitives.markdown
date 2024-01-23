---
title: The algebra of species: primitives
published: 2013-01-07T15:48:22Z
categories: math,species
tags: algebra,bag,identity,one,primitive,singleton,zero
---

<p><!-- -*- compile-command: "BlogLiteratelyD 05-algebra.markdown &gt; 05-algebra.html" -*- --> [<em>This is the fifth in a series of posts about combinatorial species. Previous posts: <a href="http://byorgey.wordpress.com/2012/10/17/and-now-back-to-your-regularly-scheduled-combinatorial-species/">And now, back to your regularly scheduled combinatorial species</a>; <a href="http://byorgey.wordpress.com/2012/10/27/decomposing-data-structures/">Decomposing data structures</a>; <a href="https://byorgey.wordpress.com/2012/11/20/combinatorial-species-definition/">Combinatorial species definition</a>, <a href="http://byorgey.wordpress.com/2012/12/06/species-definition-clarification-and-exercises/">Species definition clarification and exercises</a>.</em>]</p>
<p><a href="http://byorgey.wordpress.com/2012/11/20/combinatorial-species-definition/">Recall</a> that a <em>species</em> is a functor from $latex \mathbb{B}$, the category of finite sets and bijections, to $latex \mathbb{E}$,<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup> the category of finite sets and total functions. (Equivalently, species are endofunctors on $latex \mathbb{B}$, but in this post I’m going to want to think about them as the former.) That is, a species $latex F$ is a mapping sending every set of labels $latex U$ to a set of structures $latex F[U]$, which also lifts relabelings $latex \sigma : U \leftrightarrow V$ to functions $latex F[\sigma] : U \to V$ in a way that respects the compositional structure of bijections.</p>
<p>However, as I hinted in a previous post, it’s inconvenient to work directly with this definition in practice. Instead, we use an algebraic theory that lets us compositionally build up certain species from a collection of primitive species and species operations. (It’s important to note that it does <em>not</em> allow us to build <em>all</em> species, but it does allow us to build many of the ones we care about.)</p>
<p>In this post we’ll begin by examining a few natural species to take as primitive.</p>
<ul>
<li><p>The <em>zero</em> or <em>empty</em> species, denoted $latex \mathbf{0}$, is the unique species with no structures whatsoever; that is,</p>
<p>$latex \mathbf{0}[U] = \emptyset$</p>
<p>and</p>
<p>$latex \mathbf{0}[\sigma : U \leftrightarrow V] = id_{\emptyset} : \mathbf{0}[U] \to \mathbf{0}[V]$.</p>
<p>Of course, $latex \mathbf{0}$ will turn out to be the identity element for species sum (which I’ll define in my next post, though it’s not hard to figure out what it should mean).</p></li>
<li><p>The <em>unit</em> species, denoted $latex \mathbf{1}$, is defined by</p>
<p>$latex \begin{array}{lcl}\mathbf{1}[\emptyset] &amp;=&amp; \{\star\} \\ \mathbf{1}[U] &amp;=&amp; \emptyset \qquad (U \neq \emptyset)\end{array}$</p>
<p>That is, there is a unique $latex \mathbf{1}$-structure indexed by the empty set of labels, and no $latex \mathbf{1}$-structures with any positive number of labels. $latex \mathbf{1}$ lifts bijections in the obvious way, sending every bijection to the appropriate identity function.</p>
<p>Some people initially find this definition surprising, expecting something like $latex \mathbf{1}[U] = \{ \star \}$ for all $latex U$ instead. That is indeed a valid species, and we will meet it below; but as I hope you will come to see, it doesn’t deserve the name $latex \mathbf{1}$.</p>
<p>Of course we should also verify that this definition satisfies the requisite functoriality properties, which is not difficult.</p>
<p>More abstractly, for those who know some category theory, it’s worth mentioning that $latex \mathbf{1}$ can be defined as $latex \mathbb{B}(\emptyset, -) : \mathbb{B} \to \mathbb{E}$, that is, the covariant hom-functor sending each finite set $latex U \in \mathbb{B}$ to the (finite) set of bijections $latex \emptyset \leftrightarrow U$. (This is why I wanted to think of species as functors $latex \mathbb{B} \to \mathbb{E}$. I learned this fact from Yeh (1986).) There is, of course, a unique bijection $latex \emptyset \leftrightarrow \emptyset$ and no bijections $latex \emptyset \leftrightarrow U$ for nonempty $latex U$, thus giving rise to the definition above.</p>
<p>As you might expect, $latex \mathbf{1}$ will be the identity element for species product. Like $latex \mathbf{1}$ itself, species product isn’t defined quite as most people would initially guess. If you haven’t seen it before, you might like to try working out how product can be defined in order to make $latex \mathbf{1}$ an identity element.</p></li>
<li><p>The <em>singleton</em> species, denoted $latex \mathbf{X}$, is defined by</p>
<p>$latex \mathbf{X}[U] = \begin{cases} U &amp; |U| = 1 \\ \emptyset &amp; \text{otherwise} \end{cases}$</p>
<p>with lifting of bijections defined in the evident manner. That is, there is a single $latex \mathbf{X}$-structure on a label set of size $latex 1$ (which we identify with the label itself, though we could have also defined $latex \mathbf{X}[U] = \{\star\}$ when $latex |U| = 1$), and no $latex \mathbf{X}$-structures indexed by any other number of labels.</p>
<p>As with $latex \mathbf{1}$, we may equivalently define $latex \mathbf{X}$ as a hom-functor, namely $latex \mathbf{X} = \mathbb{B}(\{\star\}, -)$.</p>
<p>It’s worth noting again that although $latex \mathbf{1}$ and $latex \mathbf{X}$ do “case analysis” on the label set $latex U$, they actually only depend on the <em>size</em> of $latex U$; indeed, as we <a href="http://byorgey.wordpress.com/2012/12/06/species-definition-clarification-and-exercises/">noted previously</a>, by functoriality this is all they can do.</p></li>
<li><p>The species of <em>bags</em><sup><a href="#fn2" class="footnoteRef" id="fnref2">2</a></sup>, denoted $latex \mathbf{E}$, is defined by</p>
<p>$latex \mathbf{E}[U] = \{U\}$,</p>
<p>that is, there is a single $latex \mathbf{E}$-structure on any set of labels $latex U$, which we usually identify with the set of labels itself (although we could equivalently define $latex \mathbf{E}[U] = \{\star\}$). The idea is that an $latex \mathbf{E}$-structure consists solely of a collection of labels, with no imposed ordering whatsoever.</p>
<p>If you want to abuse types slightly, one can define $latex \mathbf{E}$ as a hom-functor too, namely $latex \mathbb{E}(-,\{\star\})$. (Yeh (1986) actually has $latex \mathbb{B}(-, \{\star\})$, but that’s wrong.)</p></li>
</ul>
<p>As a summary, here’s a graphic showing $latex \mathbf{0}$-, $latex \mathbf{1}$-, $latex \mathbf{X}$-, and $latex \mathbf{E}$-structures arranged by size (<em>i.e.</em>, the size of the underlying set of labels $latex U$): a dot indicates a single structure, and the size of the label set increases as you move to the right.</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2013/01/a3f68a37ffc133339fc3b8f097570c28.png" /><p class="caption"></p>
</div>
</div>
<p>Just as a teaser, it turns out that $latex \mathbf{X}$ and $latex \mathbf{E}$ are identity elements for certain binary operations on species as well, though you’ll have to wait to find out which ones!</p>
<p>Next up, addition!</p>
<h2 id="references">References</h2>
<p>Yeh, Yeong-Nan. 1986. “The calculus of virtual species and K-species.” In <em>Combinatoire énumérative</em>, ed. Gilbert Labelle and Pierre Leroux, 1234:351–369. Springer Berlin Heidelberg. <a href="http://dx.doi.org/10.1007/BFb0072525" title="http://dx.doi.org/10.1007/BFb0072525">http://dx.doi.org/10.1007/BFb0072525</a>.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>Last time I called this category $latex \mathbf{FinSet}$, but $latex \mathbb{E}$ is more concise and matches the species literarure.<a href="#fnref1">↩</a></p></li>
<li id="fn2"><p>The species literature calls this the species of <em>sets</em>, but that’s misleading to computer scientists, who expect the word “set” to imply that elements cannot be repeated.<a href="#fnref2">↩</a></p></li>
</ol>
</div>

