---
title: 'Virtual species suffice'
published: 2017-02-10T18:11:39Z
categories: combinatorics
tags: species,virtual
---

<p>Over six years ago, I <a href="https://byorgey.wordpress.com/2010/11/24/species-subtraction-made-simple/">wrote a post</a> explaining how <em>virtual species</em> are defined. Ever since then (time flies!) I’ve been meaning to write a follow-up post explaining a bit more about virtual species and how they actually suffice to give us not just additive inverses, but also (somewhat surprisingly) <em>multiplicative</em> inverses.</p>
<p>Recall that the intuitive idea of a combinatorial species is a <em>family of labelled structures</em> which are invariant under relabelling. If you’ve never seen the formal definition before, don’t worry: just think “data structures” or “algebraic data types” for now.</p>
<p>The basic idea of <em>virtual species</em> is to work with <em>pairs</em> of species $(P,N)$ where $P$ is considered “positive” and $N$ “negative”. Formally, we consider equivalence classes of such pairs under the equivalence relation defined by $(P_1, N_1) \cong (P_2, N_2)$ iff $P_1 + N_2 = P_2 + N_1$.<a href="#fn1" class="footnoteRef" id="fnref1"><sup>1</sup></a> This parallels the way one typically gives a formal definition of the integers starting from the natural numbers (the “Grothendieck construction”); see <a href="https://byorgey.wordpress.com/2010/11/24/species-subtraction-made-simple/">my previous post</a> for more details.</p>
<h1 id="intuition">Intuition</h1>
<p>How can we build intuition for virtual species, and for additive inverses of species in particular? To be honest I have been struggling with this question for many years.</p>
<p><em>Multiplicative</em> inverses are much simpler to think about: they are like matter and antimatter. Having <em>both</em> an $F$-structure and an $F^{-1}$ structure is the same as having <em>nothing</em>; they annihilate each other. By “having nothing” we mean “having no information”, that is, having a unit value: $F F^{-1} = 1$.</p>
<p>What about <em>additive</em> inverses? Note first that the $0$ species does not correspond to <em>having nothing</em>; the word “nothing” corresponds to the $1$ (<em>i.e.</em> unit) species. Instead the $0$ (<em>i.e.</em> uninhabited) species corresponds to <em>(logical) impossibility</em>. So to interpret $-F$ we have to imagine something where <em>having either $F$ or $-F$ is impossible</em>.</p>
<p>…yeah, me neither. This seems deeply strange. If someone says, “I either have an $F$ or a $-F$”, you can confidently call them a liar, because it is impossible to have either an $F$ or a $-F$; that is, $F - F = 0$. But surely if you actually have an $F$-structure, it should also be true to say “I have either an $F$ or a $G$”? Well, that works for normal, positive species—in which case we can define a canonical injection $F \to F + G$. But once we introduce negative species this completely breaks down. As another example, if someone <em>truthfully</em> says, “I have either a tree or a negative non-empty tree”, you should be able to say, “Aha! I know what you have—it must be an empty tree.” In general, it’s strange that expressing a <em>disjunction</em> can cause some possibilities to be <em>ruled out</em>. Normally, we are used to disjunctions only <em>increasing</em> the number of possibilities.</p>
<p>Inspired by James and Sabry’s really cool paper <a href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.399.3417&amp;rep=rep1&amp;type=pdf">The Two Dualities of Computation: Negative and Fractional Types</a>, I have thought a bit about whether there is some plausible interpretation involving travelling backwards in time, but I haven’t been able to come up with one. I can’t quite seem to make the formalism of the paper match up with my intuition about species (though this may just be a failure of my imagination or intuition).</p>
<h1 id="multiplicative-inverses">Multiplicative Inverses</h1>
<p>In any case, let’s see why the ring of virtual species actually has multiplicative inverses—at least, all the ones we could possibly hope for. This is somewhat surprising, since when we build integers from natural numbers by considering equivalence classes of pairs, we certainly don’t get any multiplicative inverses, only additive ones. To get multiplicative inverses we have to do the same process a second time, building the rational numbers as equivalence classes of pairs of integers. But species already have enough extra structure that throwing in additive inverses is all it takes.</p>
<p>First, a caveat: we don’t get multiplicative inverses for <em>all</em> species, but only those species $G$ such that $G(0) = 1$: that is, species $G$ with only a single structure of size zero, which are of the form $G = 1 + X(\dots)$. With any constant term other than $1$, we clearly have no hope of finding another species $H$ such that $GH = 1$, since the constant term of $GH$ will be a multiple of $G$’s constant term.</p>
<p>So given such a $G$, write $G = 1 + G_+$, where $G_+$ denotes “non-empty $G$-structures”. Then we can define the multiplicative inverse of $G$ as follows:</p>
<p><div style="text-align:center;">
$\displaystyle G^{-1} = \sum_{k \geq 0} (-1)^k (G_+)^k = 1 - G_+ + G_+^2 - G_+^3 + \dots$
</div></p>
<p>That is, a $G^{-1}$-structure consists of a list of nonempty $G$-structures, except that even-length lists are considered “positive” and odd-length lists considered “negative”.</p>
<p>We can easily check that this indeed defines a multiplicative inverse for $G$:</p>
<p><div style="text-align:center;">
$\displaystyle \begin{array}{rcl}G G^{-1} &amp;=&amp; (1 + G_+) (1 - G_+ + G_+^2 - G_+^3 + \dots) \\[0.5em] &amp;=&amp; (1 - G_+ + G_+^2 - G_+^3 + \dots) + (G_+ - G_+^2 + G_+^3 - G_+^4 + \dots) \\[0.5em] &amp;=&amp; 1 \end{array}$
</div></p>
<p>The infinite sums telescope down to leave only $1$. Notice this really isn’t about species in particular, but really about infinite power series (of which species are the categorification): any infinite power series with integer coefficients and a constant term of $1$ has a multiplicative inverse which is also such a power series.</p>
<p>As an example, consider $1/(1-X) = (1-X)^{-1}$. We know this is “supposed” to be the species of lists (since it results from solving $L = 1 + XL$ for $L$), but let’s see what happens. In this case $G = 1-X$ and $G_+ = -X$. So the inverse ought to be</p>
<p><div style="text-align:center;">
$\displaystyle (1-X)^{-1} = \sum_{k \geq 0} (-1)^k (-X)^k = \sum_{k \geq 0} X^k = 1 + X + X^2 + X^3 + \dots$
</div></p>
<p>And hey, look at that! Lists!</p>
<h1 id="a-field-of-species">A field of species?</h1>
<p>So what would we need to get a true field, <em>i.e.</em> a multiplicative inverse for <em>every</em> nonzero species? Well, for that we would need to throw in rational coefficients. I forget exactly where I read this—some paper by Baez and Dolan, most likely—but I believe the proper way to interpret this would be as groupoid-valued species, since there is a sense in which the “cardinality” of groupoids can be interpreted as rational numbers. But to be honest I have no idea where this leads.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>Note that species sum is cancellative—that is, if $A + C = B + C$ then $A = B$—so this is a coherent definition. This cancellative property is probably worth another post of its own since the reason for it is not entirely trivial.<a href="#fnref1">↩</a></p></li>
</ol>
</div>

