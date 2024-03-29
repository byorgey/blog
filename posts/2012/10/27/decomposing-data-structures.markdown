---
katex: true
title: 'Decomposing data structures'
published: 2012-10-28T03:01:07Z
categories: math,species
tags: data structures,labels,locations,shapes
---

<p>So, what are <em>combinatorial species</em>? As a very weak first approximation, you can think of them as a <em>generalization of algebraic data types</em>.<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup> That doesn’t really say much about what they are, but at least it does explain why programmers might be interested in them.</p>
<p>The goal of species is to have a unified theory of <em>structures</em>, or <em>containers</em>. By a <em>structure</em> we mean some sort of “shape” containing <em>locations</em> (or <em>positions</em>). Here are two different structures, each with eight locations:</p>

<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/10/82e8948fc7048c08ac1eaaa1d3dadb5b.png" /><p class="caption"></p>
</div>
</div>
<p>One thing that’s important to get straight from the beginning is that we are talking about structures with <em>labeled locations</em>. The <strong>numbers in the picture above are <em>not</em> data</strong> being stored in the structures, but <em>names</em> or <em>labels</em> for the locations. To talk about a <em>data structure</em> (<em>i.e.</em> a structure filled with data), we would have to also specify a mapping from locations to data, like $\{ 0 \mapsto \texttt{'s'}, 1 \mapsto \texttt{'p'}, 2 \mapsto \texttt{'e'} \dots \}$</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/10/24ee86a859a0b6a3a54bcf3f6a1188df.png" /><p class="caption"></p>
</div>
</div>
<p>Now go reread the above paragraph! For programmers I find that this is one of the most difficult things to grasp at first—or at least one of the things that is easiest to forget. The fact that the labels are often natural numbers (which are often also used as sample data) does not help.</p>
<p>One useful intuition is to think of the labels as <em>memory addresses</em>, which point off to some location where a data value is stored. This intuition has some particularly interesting consequences when we get to talking about operations like Cartesian product and functor composition, since it gives us a way to model sharing (albeit only in limited ways).</p>
<p>Why have labels at all? In the tree shown above, we can uniquely identify each location by a path from the root of the tree, without referencing their labels at all. However, the other structure illustrates one reason why labels are needed. The circle is supposed to indicate that the structure has <em>rotational symmetry</em>, so there would be no way to uniquely refer to any location other than by giving them labels.</p>
<p>The idea of decomposing data structures as shapes with locations combined with data is not unique to species. In the computer science community, the idea goes back, I think, to Jay and Cockett (1994) in their work on “shapely types” (their “locations” are always essentially natural numbers, since they work in terms of shapes and <em>lists</em> of data) and more recently Abbott, Altenkirch, and Ghani (2003) with their definition of “containers” (which, like the theory of species, has a much more general notion of locations). However, it should be noted that the literature on species never actually talks about mappings from labels to data: combinatorialists don’t care about data structures, they only care about structures!</p>
<p>Now that we have some motivation, and with the requisite disclaimers about labels out of the way, in my next post I’ll motivate and explain the formal definition of species.</p>
<h2 id="references">References</h2>
<p>Abbott, Michael, Thorsten Altenkirch, and Neil Ghani. 2003. “Categories of Containers.” In <em>Foundations of Software Science and Computation Structures</em>, 23–38. <a href="http://dx.doi.org/10.1007/3-540-36576-1_2" title="http://dx.doi.org/10.1007/3-540-36576-1_2">http://dx.doi.org/10.1007/3-540-36576-1_2</a>.</p>
<p>Jay, C. Barry, and J. Robin B. Cockett. 1994. “Shapely Types and Shape Polymorphism.” In <em>ESOP ’94: Proceedings of the 5th European Symposium on Programming</em>, 302–316. London, UK: Springer-Verlag.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>No relation to <a href="http://www.haskell.org/haskellwiki/GADT">Generalized Algebraic Data Types</a>.<a href="#fnref1">↩</a></p></li>
</ol>
</div>

