---
title: Avoiding the axiom of choice, part I
published: 2014-05-08T15:59:54Z
categories: category theory,math,species
tags: AC,axiom of choice,category,constructive,theory,types
---

<p>I’m hard at work on <a href="http://github.com/byorgey/thesis">my dissertation</a>, and plan to get back to doing a bit of blogging based on stuff I’m writing and thinking about, as a way of forcing myself to explain things clearly and to potentially get some useful feedback. Yes, this means you can expect more on <a href="http://byorgey.wordpress.com/2013/01/07/the-algebra-of-species-primitives/">species</a>! But first, I’ll write a few posts about the <a href="http://en.wikipedia.org/wiki/Axiom_of_choice"><em>axiom of choice</em></a> and <a href="http://ncatlab.org/nlab/show/anafunctor"><em>anafunctors</em></a>, which have turned out to be fundamental to some of the work I’ve been doing. If you look at the nLab page for anafunctors, linked above, you could be forgiven for thinking this is a pretty esoteric corner of category theory, but in fact it’s not too bad once you grasp the essentials, and is quite relevant for anyone interested in category theory and constructive/computational foundations (such as <a href="http://homotopytypetheory.org/">homotopy type theory (HoTT)</a>).</p>
<h2 id="the-axiom-of-choice">The Axiom of Choice</h2>
<p>The (in)famous <em>Axiom of Choice</em> (hereafter, AC) can be formulated in a number of equivalent ways. Perhaps the most well-known is:</p>
<div style="text-align:center;">
<em>The Cartesian product of any collection of non-empty sets is non-empty.</em>
</div>
<p><br /></p>
<p>Given a family of sets $latex \{X_i \mid i \in I\}$, an element of their Cartesian product is some $latex I$-indexed tuple $latex \{x_i \mid i \in I\}$ where $latex x_i \in X_i$ for each $latex i$. Such a tuple can be thought of as a function (called a <em>choice function</em>) which picks out some particular $latex x_i$ from each $latex X_i$.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2014/05/b6d54a1a96c16b46.png" /></p>
</div>
<h2 id="the-axiom-of-choice-in-type-theory-take-1">The Axiom of Choice in type theory, take 1</h2>
<p>We can express this in type theory as follows. First, we assume we have some type $latex I$ which indexes the collection of sets; that is, there will be one set for each value of type $latex I$. Given some type $latex A$, we can define a subset of the values of type $latex A$ using a <em>predicate</em>, that is, a function $latex P : A \to \star$ (where $latex \star$ denotes the universe of types). For some particular $latex a : A$, applying $latex P$ to $latex a$ yields a type, which can be thought of as the type of <em>evidence</em> that $latex a$ is in the subset $latex P$; $latex a$ is in the subset if and only if $latex P\ a$ is inhabited. An $latex I$-indexed collection of subsets of $latex A$ can then be expressed as a function $latex C : I \to A \to \star$. In particular, $latex C(i,a)$ is the type of evidence that $latex a$ is in the subset indexed by $latex i$. (Note that we could also make $latex A$ into a family of types indexed by $latex I$, that is, $latex A : I \to \star$, but it wouldn’t add anything to this discussion.)</p>
<p>A set is nonempty if it has at least one element, so the fact that all the sets in $latex C$ are nonempty can be modeled by a dependent function which yields an element of $latex A$ for each index, along with a proof that it is contained in the corresponding subset.</p>
<p>$latex (i : I) \to (a : A) \times C(i,a)$</p>
<p>(Note I’m using the notation $latex (x:X) \to T(x)$ for dependent function types instead of $latex \prod_{x:X} T(x)$, and $latex (x:X) \times T(x)$ for dependent pairs instead of $latex \sum_{x:X} T(x)$.) An element of the Cartesian product of $latex C$ can be expressed as a function $latex I \to A$ that picks out an element for each $latex I$ (the choice function), together with a proof that the chosen elements are in the appropriate sets:</p>
<p>$latex (g : I \to A) \times ((i : I) \to C(i, g(i)))$</p>
<p>Putting these together, apparently the axiom of choice can be modelled by the type</p>
<p>$latex ((i : I) \to (a : A) \times C(i,a)) \to (g : I \to A) \times ((i : I) \to C(i, g(i)))$</p>
<p>Converting back to $latex \Pi$ and $latex \Sigma$ notation and squinting actually gives some good insight into what is going on here:</p>
<p>$latex \left( \prod_{i : I} \sum_{a : A} C(i,a) \right) \to \left( \sum_{g : I \to A} \prod_{i : I} C(i, g(i)) \right)$</p>
<p>Essentially, this says that we can “turn a (dependent) product of sums into a (dependent) sum of products”. This sounds a lot like distributivity, and indeed, the strange thing is that this is simply <em>true</em>: implementing a function of this type is a simple exercise! If you aren’t familiar with dependent type theory, you can get the intuitive idea by implementing a non-dependent Haskell analogue, namely something of type</p>
<p><code>(i -&gt; (a,c)) -&gt; (i -&gt; a, i -&gt; c)</code>.</p>
<p>Not too hard, is it? (The implementation of the dependent version is essentially the same; it’s only the types that get more complicated, not the implementation.) So what’s going on here? Why is AC so controversial if it is simply <em>true</em> in type theory?</p>
<h2 id="the-axiom-of-choice-in-type-theory-take-2">The Axiom of Choice in type theory, take 2</h2>
<p><em>This is not the axiom of choice you’re looking for.</em> — Obi-Wan Funobi</p>
<p>The problem, it turns out, is that we’ve modelled the axiom of choice improperly, and it all boils down to how <em>non-empty</em> is defined. When a mathematician says “$latex S$ is non-empty”, they typically don’t actually mean “…and here is an element of $latex S$ to prove it”; instead, they literally mean “it is <em>not the case</em> that $latex S$ is empty”, that is, assuming $latex S$ is empty leads to a contradiction. (Actually, it is a bit more subtle yet, but this is a good first approximation.) In classical logic, these viewpoints are equivalent; in constructive logic, however, they are very different! In constructive logic, knowing that it is a contradiction for $latex S$ to be empty does not actually help you find an element of $latex S$. We modelled the statement “this collection of non-empty sets” essentially by saying “here is an element in each set”, but in constructive logic that is a much <em>stronger</em> statement than simply saying that each set is not empty.</p>
<p>(I should mention at this point that when working in HoTT, the best way to model what classical mathematicians mean when they say “$latex S$ is non-empty” is probably not with a negation, but instead with the <em>propositional truncation</em> of the statement that $latex S$ contains an element. Explaining this would take us too far afield; if you’re interested, you can find details in Chapter 3 of <a href="http://homotopytypetheory.org/book/">the HoTT book</a>, where all of this and much more is explained in great detail.)</p>
<p>From this point of view, we can see why the “AC” in the previous section was easy to implement: it had to produce a function choosing a bunch of elements, but it was given a bunch of elements to start! All it had to do was shuffle them around a bit. The “real” AC, on the other hand, has a much harder job: it is told some sets are non-empty, but without any actual elements being mentioned, and it then has to manufacture a bunch of elements out of thin air. This is why it has to be taken as an axiom; we can also see that it doesn’t fit very well in a constructive/computational context. Although it is logically consistent to assume it as an axiom, it has no computational interpretation, so anything we define using it will just get stuck operationally.</p>
<p>So, we’ll just avoid using AC. No problem, right?</p>
<h2 id="dont-look-now-but-ac-is-behind-you">Don’t look now, but AC is behind you</h2>
<p>The problem is that AC is really sneaky. It tends to show up all over the place, but disguised so that you don’t even realize it’s there. You really have to train yourself to think in a fundamentally constructive way before you start to notice the places where it is used. Next time I’ll explain one place it shows up a lot, namely, when defining functors in category theory (though thankfully, not when defining <code>Functor</code> instances in Haskell).</p>
<div class="references">

</div>

