---
katex: true
title: 'Combinatorial species definition'
published: 2012-11-20T17:00:42Z
categories: math,species
tags: bijection,data structures,definition,functor,labels
---

<p><!-- -*- compile-command: "BlogLiteratelyD 03-defn.markdown &gt; 03-defn.html" -*- --> Continuing from <a href="http://byorgey.wordpress.com/2012/10/27/decomposing-data-structures/">my previous post</a>, recall that the goal of species is to have a unified theory of <em>containers</em> with <em>labeled<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup> locations</em>. So, how do we actually specify such things (leaving aside for the moment the question of how we <em>compute</em> with them)?</p>
<p>We might imagine specifying them by:</p>
<ul>
<li>using any arbitrary set to represent some family of labeled structures (<em>e.g.</em> the set of labeled binary tree structures, the set of labeled list structures, …), together with</li>
<li>a function that takes a structure and computes its set of labels.</li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/11/bf68c3f079dd47e1fdff8761e7f4c910.png" /><p class="caption"></p>
</div>
</div>
<p>On the face of it this seems quite natural (at least, it does to me). However, it works better to instead use a function <em>from</em> sets of labels to <em>the subset of all structures containing precisely those labels</em>.</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/11/dcc9ae3b9e3b449bdb46970bd943ea66.png" /><p class="caption"></p>
</div>
</div>
<p>In my experience teaching people about species, this often seems to be a source of confusion—it seems “backwards”. More generally, when thinking about a set $B$ <em>indexed by</em> some other set $A$ (in this case, structures indexed by their sets of labels), one might think to model this by a function $B \to A$ (which tells us the index), but it actually works better to model it by a function $A \to B$, which takes each “index” to the set of all things indexed by it.<sup><a href="#fn2" class="footnoteRef" id="fnref2">2</a></sup> Hopefully as we work through the rest of the definition you’ll get a sense for why it works better this way. For now, I think the best advice is <strong>don’t assign computational significance</strong> to these functions from labels to structures. Just think of them as a convenient technical device to keep track of shapes indexed by labels.</p>
<p>In any case, the first half of the definition is:</p>
<ul>
<li>A <em>species</em> $F$ is a mapping from sets of labels to sets of structures.</li>
</ul>
<p>(I deliberately chose the word <em>mapping</em> instead of <em>function</em> to emphasize, again, that we don’t particularly want to assign it computational significance.) Of course, the fact that a species takes sets “of labels” as input and outputs sets “of structures” doesn’t matter; any sets will do, so we might as well just say that a species maps sets to sets. We write $F[U]$ for the species $F$ applied to a set of labels $U$, and call $F[U]$ the set of “$F$-structures with labels drawn from $U$”, or simply “$F$-structures on $U$”, or even (when $U$ is clear from context) just “$F$-structures”.</p>
<p>So far, however, this is rather uninteresting, and moreover it fails to adequately capture our intuition for what “structures” are. Intuitively, the labels are incidental, just like the variable names used in lambda terms are incidental: we must use them to be able to distinguish locations, but the precise objects we use as labels really “shouldn’t matter”. That is, given two sets of labels of the same size, we ought to have “the same” family of structures indexed by each. Of course they can’t be <em>literally</em> the same, because they have different labels! But they should be the same “up to relabeling”. We want to rule out the ability to have two same-size sets of labels indexing wildly different sets of structures: a species shouldn’t be able to “look at” the individual labels in order to “decide” what sort of structures to produce, just like a polymorphic type in Haskell can’t “look at” its type argument. The major difference is that species <em>are</em> allowed to “look at” the <em>size</em> of the label set.</p>
<p>Making this intuition precise is the clever part, and is really the pivotal point around which the whole theory revolves. Here’s how we do it. We don’t work with sizes of label sets directly; instead we work with <em>bijections</em> between label sets. (Of course, if there is a bijection between two finite sets then they are necessarily the same size.)</p>
<p>Given two label sets $U$ and $V$ which are related by the bijection $\sigma$ (sometimes referred to as a <em>relabeling</em>), there must be a relationship between $F[U]$ and $F[V]$—in particular they must also be in bijection. Here, then, is the second part of the definition:</p>
<ul>
<li>Given a bijection $\sigma : U \leftrightarrow V$, a species $F$ must also “lift” $\sigma$ to a bijection $F[\sigma] : F[U] \leftrightarrow F[V]$.</li>
</ul>
<p>(Note that we’re recycling notation here, using $F[-]$ for the action of species on both label sets and bijections.) However, this still isn’t quite enough: we don’t want $F[\sigma]$ to be just <em>any</em> bijection between $F[U]$ and $F[V]$. It really should be the specific bijection that “applies” $\sigma$ to the labels contained within the structures in $F[U]$. For example, it would be weird if the identity relabeling, when lifted through $F$, resulted in some nontrivial reshuffling of the structures in $F[U]$. It would also be strange if $F$ didn’t respect composition, that is, if there were some $\sigma, \tau$ such that $F[\sigma] \circ F[\tau] \neq F[\sigma \circ \tau]$, since intuitively “applying $\tau$ then applying $\sigma$” ought to be the same as “applying $(\sigma \circ \tau)$”. So we add these as conditions:</p>
<ul>
<li>$F$ must map every identity bijection $id : U \leftrightarrow U$ to the identity $id : F[U] \leftrightarrow F[U]$, and</li>
<li>$F$ must preserve composition of bijections, that is, $\forall \sigma, \tau. F[\sigma \circ \tau] = F[\sigma] \circ F[\tau]$.</li>
</ul>
<p>Of course, all of this may look rather familiar if you know some basic category theory. Consider the category $\mathbb{B}$ whose objects are sets and whose morphisms are bijections. Then all of the above can be summed up by the pithy</p>

<blockquote>
<em>A species is an endofunctor on $\mathbb{B}$.</em>
</blockquote>

<p>Whew! We finally made it to the definition. However, working directly with the definition is not very convenient. In my next post I’ll begin explaining the more usual algebraic approach.</p>
<p>At this point I should mention that species were first introduced by André Joyal in his thesis (1981). Unfortunately it is in French, which I cannot read. Fortunately Bergeron, Labelle, and Leroux (1998) wrote an excellent reference text on the subject of species. Unfortunately it is in French too. Fortunately, <a href="http://www.ms.uky.edu/~readdy/">Margaret Readdy</a> translated it into English!</p>
<h2 id="references">References</h2>
<p>Bergeron, F., G. Labelle, and P. Leroux. 1998. <em>Combinatorial species and tree-like structures</em>. Trans. Margaret Readdy. <em>Encyclopedia of Mathematics and its Applications</em>. Cambridge: Cambridge University Press.</p>
<p>Joyal, André. 1981. “Une Théorie Combinatoire des Séries Formelles.” <em>Advances in Mathematics</em> 42: 1–82.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>A note on spelling: generally, “labeled” is the American spelling and “labelled” British (though “labelled” is also in common American usage, according to <a href="http://www.merriam-webster.com/dictionary/labeled">Merriam-Webster</a>). I try to consistently use the American spelling, but will probably slip up occasionally, and you should use whichever spelling makes you happiest.<a href="#fnref1">↩</a></p></li>
<li id="fn2"><p>I’ve seen this pattern show up multiple times in different category-theoretic contexts, but I don’t feel qualified to comment on it more generally. If you have any pointers to more general discussion of this idea/phenomenon I’d appreciate it.<a href="#fnref2">↩</a></p></li>
</ol>
</div>

