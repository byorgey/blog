---
title: 'Catsters guide'
published: 2014-01-13T16:35:53Z
categories: 
tags: 
---

<div id="TOC">
<ul>
<li><a href="#introduction">Introduction</a></li>
<li><a href="#terminal-and-initial-objects">Terminal and Initial objects</a><ul>
<li><a href="#terminal-and-initial-objects-1">Terminal and initial objects 1</a></li>
<li><a href="#terminal-and-initial-objects-2">Terminal and initial objects 2</a></li>
<li><a href="#terminal-and-initial-objects-3">Terminal and initial objects 3</a></li>
</ul></li>
<li><a href="#products-and-coproducts">Products and Coproducts</a><ul>
<li><a href="#products-and-coproducts-1">Products and coproducts 1</a></li>
<li><a href="#products-and-coproducts-2">Products and coproducts 2</a></li>
<li><a href="#products-and-coproducts-3">Products and coproducts 3</a></li>
<li><a href="#products-and-coproducts-4">Products and coproducts 4</a></li>
</ul></li>
<li><a href="#pullbacks-and-pushouts">Pullbacks and Pushouts</a><ul>
<li><a href="#pullbacks-and-pushouts-1">Pullbacks and pushouts 1</a></li>
<li><a href="#pullbacks-and-pushouts-2">Pullbacks and pushouts 2</a></li>
</ul></li>
<li><a href="#natural-transformations">Natural transformations</a><ul>
<li><a href="#natural-transformations-1">Natural transformations 1</a></li>
<li><a href="#natural-transformations-2">Natural transformations 2</a></li>
<li><a href="#natural-transformations-3">Natural transformations 3</a></li>
<li><a href="#natural-transformations-3a">Natural transformations 3A</a></li>
</ul></li>
<li><a href="#representable-functors-and-the-yoneda-lemma">Representable functors and the Yoneda Lemma</a><ul>
<li><a href="#representables-and-yoneda-1">Representables and Yoneda 1</a></li>
<li><a href="#representables-and-yoneda-2">Representables and Yoneda 2</a></li>
<li><a href="#representables-and-yoneda-3">Representables and Yoneda 3</a></li>
</ul></li>
<li><a href="#adjunctions-part-1">Adjunctions (part 1)</a><ul>
<li><a href="#adjunctions-1">Adjunctions 1</a></li>
<li><a href="#adjunctions-2">Adjunctions 2</a></li>
<li><a href="#adjunctions-4">Adjunctions 4</a></li>
</ul></li>
<li><a href="#monads">Monads</a><ul>
<li><a href="#monads-1">Monads 1</a></li>
<li><a href="#monads-2">Monads 2</a></li>
<li><a href="#monads-3">Monads 3</a></li>
<li><a href="#monads-3a">Monads 3A</a></li>
<li><a href="#monads-4">Monads 4</a></li>
</ul></li>
<li><a href="#adjunctions-and-monads">Adjunctions and monads</a><ul>
<li><a href="#adjunctions-3">Adjunctions 3</a></li>
<li><a href="#adjunctions-5">Adjunctions 5</a></li>
<li><a href="#adjunctions-6">Adjunctions 6</a></li>
<li><a href="#adjunctions-7">Adjunctions 7</a></li>
</ul></li>
<li><a href="#string-diagrams">String diagrams</a><ul>
<li><a href="#string-diagrams-1">String diagrams 1</a></li>
<li><a href="#string-diagrams-2">String diagrams 2</a></li>
<li><a href="#string-diagrams-3">String diagrams 3</a></li>
<li><a href="#string-diagrams-4">String diagrams 4</a></li>
<li><a href="#string-diagrams-5">String diagrams 5</a></li>
</ul></li>
<li><a href="#pipe-cleaners">Pipe cleaners</a></li>
<li><a href="#general-limits-and-colimits">General Limits and Colimits</a><ul>
<li><a href="#general-limits-and-colimits-1">General limits and colimits 1</a></li>
<li><a href="#general-limits-and-colimits-2">General limits and colimits 2</a></li>
<li><a href="#general-limits-and-colimits-3">General limits and colimits 3</a></li>
<li><a href="#general-limits-and-colimits-4">General limits and colimits 4</a></li>
<li><a href="#general-limits-and-colimits-5">General limits and colimits 5</a></li>
<li><a href="#general-limits-and-colimits-6">General limits and colimits 6</a></li>
</ul></li>
<li><a href="#slice-and-comma-categories">Slice and comma categories</a><ul>
<li><a href="#slice-and-comma-categories-1">Slice and comma categories 1</a></li>
<li><a href="#slice-and-comma-categories-2">Slice and comma categories 2</a></li>
</ul></li>
<li><a href="#coequalisers">Coequalisers</a><ul>
<li><a href="#coequalisers-1">Coequalisers 1</a></li>
<li><a href="#coequalisers-2">Coequalisers 2</a></li>
</ul></li>
<li><a href="#monoid-objects">Monoid objects</a><ul>
<li><a href="#monoid-objects-1">Monoid objects 1</a></li>
<li><a href="#monoid-objects-2">Monoid objects 2</a></li>
</ul></li>
<li><a href="#categories">2-categories</a><ul>
<li><a href="#categories-1">2-categories 1</a></li>
<li><a href="#categories-2">2-categories 2</a></li>
</ul></li>
<li><a href="#eckmann-hilton">Eckmann-Hilton</a><ul>
<li><a href="#eckmann-hilton-1">Eckmann-Hilton 1</a></li>
<li><a href="#eckmann-hilton-2">Eckmann-Hilton 2</a></li>
</ul></li>
<li><a href="#distributive-laws">Distributive laws</a><ul>
<li><a href="#distributive-laws-1">Distributive laws 1</a></li>
<li><a href="#distributive-laws-2">Distributive laws 2</a></li>
<li><a href="#distributive-laws-3-aka-monads-6">Distributive laws 3 (aka Monads 6)</a></li>
<li><a href="#distributive-laws-4">Distributive laws 4</a></li>
</ul></li>
<li><a href="#group-objects-and-hopf-algebras">Group Objects and Hopf Algebras</a><ul>
<li><a href="#group-objects-and-hopf-algebras-1">Group Objects and Hopf Algebras 1</a></li>
<li><a href="#group-objects-and-hopf-algebras-2">Group Objects and Hopf Algebras 2</a></li>
<li><a href="#group-objects-and-hopf-algebras-3">Group Objects and Hopf Algebras 3</a></li>
<li><a href="#group-objects-and-hopf-algebras-4">Group Objects and Hopf Algebras 4</a></li>
<li><a href="#group-objects-and-hopf-algebras-5">Group Objects and Hopf Algebras 5</a></li>
<li><a href="#group-objects-and-hopf-algebras-6">Group Objects and Hopf Algebras 6</a></li>
</ul></li>
<li><a href="#ends">Ends</a><ul>
<li><a href="#ends-1">Ends 1</a></li>
<li><a href="#ends-2">Ends 2</a></li>
<li><a href="#ends-3">Ends 3</a></li>
<li><a href="#ends-4">Ends 4</a></li>
</ul></li>
<li><a href="#adjunctions-from-morphisms">Adjunctions from morphisms</a><ul>
<li><a href="#adjunctions-from-morphisms-1">Adjunctions from morphisms 1</a></li>
<li><a href="#adjunctions-from-morphisms-2">Adjunctions from morphisms 2</a></li>
<li><a href="#adjunctions-from-morphisms-3">Adjunctions from morphisms 3</a></li>
<li><a href="#adjunctions-from-morphisms-4">Adjunctions from morphisms 4</a></li>
<li><a href="#adjunctions-from-morphisms-5">Adjunctions from morphisms 5</a></li>
</ul></li>
<li><a href="#double-categories">Double Categories</a><ul>
<li><a href="#double-categories-1">Double Categories</a></li>
</ul></li>
<li><a href="#spans">Spans</a><ul>
<li><a href="#spans-1">Spans 1</a></li>
<li><a href="#spans-2">Spans 2</a></li>
</ul></li>
<li><a href="#multicategories">Multicategories</a><ul>
<li><a href="#multicategories-1">Multicategories 1</a></li>
<li><a href="#multicategories-2">Multicategories 2</a></li>
</ul></li>
<li><a href="#metric-spaces-and-enriched-categories">Metric Spaces and Enriched Categories</a><ul>
<li><a href="#metric-spaces-and-enriched-categories-1">Metric Spaces and Enriched Categories 1</a></li>
<li><a href="#metric-spaces-and-enriched-categories-2">Metric Spaces and Enriched Categories 2</a></li>
<li><a href="#metric-spaces-and-enriched-categories-3">Metric Spaces and Enriched Categories 3</a></li>
</ul></li>
</ul>
</div>

<h2 id="introduction">Introduction</h2>
<p>In an attempt to solidify and extend my knowledge of category theory, I have been working my way through the excellent <a href="http://www.youtube.com/user/TheCatsters">series of category theory lectures posted on Youtube</a> by <a href="http://www.cheng.staff.shef.ac.uk/">Eugenia Cheng</a> and <a href="http://www.simonwillerton.staff.shef.ac.uk/">Simon Willerton</a>, aka <a href="http://ncatlab.org/nlab/show/The+Catsters">the Catsters</a>.</p>
<p>Edsko de Vries used to have a listing of the videos, but it is no longer available. After wresting a copy from a Google cache, I began working my way through the videos, but soon discovered that Edsko’s list was organized by subject, <em>not</em> topologically sorted. So I started making my own list, and have put it up here in the hopes that it may be useful to others. Suggestions, corrections, improvements, <em>etc.</em> are of course welcome!</p>
<p>As far as possible I have tried to arrange the order so that each video only depends on concepts from earlier ones. Along with each video you can also find my cryptic notes; I make no guarantee that they will be useful to anyone (even me!), but hopefully they will at least give you an idea of what is in each video. (For some of the earlier videos I didn’t take notes, so I have just copied the description from YouTube.)</p>
<p>I have <a href="https://www.beeminder.com/byorgey/goals/catsters">a goal</a> to watch two videos per week (at which rate it will take me about nine months to watch all of them); I will keep this list updated with new video links and notes as I go.</p>
<h2 id="terminal-and-initial-objects">Terminal and Initial objects</h2>
<h3 id="terminal-and-initial-objects-1">Terminal and initial objects 1</h3>
<p>http://youtu.be/yeQcmxM2e5I</p>
<ul>
<li>Definition and examples of terminal objects</li>
<li>Sketch of proof that terminal objects are unique up to unique isomorphism</li>
</ul>
<h3 id="terminal-and-initial-objects-2">Terminal and initial objects 2</h3>
<p>http://youtu.be/9vhWpOVPlIE</p>
<ul>
<li>Proof that terminal objects are unique</li>
<li>Examples of categories <em>without</em> terminal objects</li>
</ul>
<h3 id="terminal-and-initial-objects-3">Terminal and initial objects 3</h3>
<p>http://youtu.be/yaPwKu5fHqI</p>
<ul>
<li>Definition and examples of initial objects</li>
</ul>
<h2 id="products-and-coproducts">Products and Coproducts</h2>
<h3 id="products-and-coproducts-1">Products and coproducts 1</h3>
<p>http://youtu.be/upCSDIO9pjc</p>
<ul>
<li>Definition of products</li>
<li>Example: cartesian product of sets</li>
<li>$B \times A$ and $A \times B$ as two (isomorphic) products</li>
<li>Uniqueness up to unique isomorphism</li>
</ul>
<h3 id="products-and-coproducts-2">Products and coproducts 2</h3>
<p>http://youtu.be/BqRkULEhG40</p>
<ul>
<li>More on uniqueness up to unique isomorphism</li>
<li>Examples and non-examples of products</li>
</ul>
<h3 id="products-and-coproducts-3">Products and coproducts 3</h3>
<p>http://youtu.be/TCQWHOUBwDE</p>
<ul>
<li>Definition and example of coproduct</li>
</ul>
<h3 id="products-and-coproducts-4">Products and coproducts 4</h3>
<p>http://youtu.be/n6nBgszToBE</p>
<ul>
<li>Definition of the morphisms $(f,g)$ and $f \times g$</li>
<li>The diagonal</li>
<li>Products with the terminal object</li>
</ul>
<h2 id="pullbacks-and-pushouts">Pullbacks and Pushouts</h2>
<h3 id="pullbacks-and-pushouts-1">Pullbacks and pushouts 1</h3>
<p>http://youtu.be/XGysPJvCXOc</p>
<ul>
<li>Definition of pullback</li>
<li>Example: pullbacks in $\mathbf{Set}$</li>
</ul>
<h3 id="pullbacks-and-pushouts-2">Pullbacks and pushouts 2</h3>
<p>http://youtu.be/LkkallToFQ0</p>
<ul>
<li>Definition of pushouts</li>
<li>Example: pushouts in $\mathbf{Set}$</li>
<li>Pullback/pushout example of intersection/union of sets</li>
</ul>
<h2 id="natural-transformations">Natural transformations</h2>
<h3 id="natural-transformations-1">Natural transformations 1</h3>
<p>http://www.youtube.com/watch?v=FZSUwqWjHCU</p>
<ul>
<li>Definition of natural transformations.</li>
<li>Naturality squares.</li>
<li>Intuition about natural transformations based on homotopy.</li>
<li>Alternative definition of natural transformation analogous to usual homotopy definition: a natural transformation is a functor $C \times  I \to D$ where $I$ is the “categorical interval”, <em>i.e.</em> the two-object category with a single nontrivial morphism.</li>
</ul>
<h3 id="natural-transformations-2">Natural transformations 2</h3>
<p>http://youtu.be/XnrqHd39Cl0</p>
<ul>
<li>Vertical composition of natural transformations.</li>
<li>Functor categories.</li>
<li>Horizontal composition of natural transformations
<ul>
<li>Note there are two ways to define horizontal composition, and they are equal by naturality.</li>
</ul></li>
</ul>
<h3 id="natural-transformations-3">Natural transformations 3</h3>
<p>http://youtu.be/EG5xUYXHFeU</p>
<ul>
<li>Whiskering (though they don’t call it that yet).</li>
<li>Horizontal composition as vertical composition of two whiskerings (in two different ways, which are equal by naturality).</li>
<li>Interchange law: proof by commutativity of whiskering.</li>
</ul>
<h3 id="natural-transformations-3a">Natural transformations 3A</h3>
<p>http://youtu.be/fsfzEz6qAGQ</p>
<ul>
<li>Define terminology “whiskering”.</li>
<li>Note vertical composition of “right-whiskered fish” works because of functoriality (of functor we whiskered by).</li>
<li>Vertical composition of “left-whiskered fish” comes by definition of vectical composition for natural transformations.</li>
<li>So in the end, interchange depends on three things: definition of vertical composition; functoriality; naturality.</li>
</ul>
<h2 id="representable-functors-and-the-yoneda-lemma">Representable functors and the Yoneda Lemma</h2>
<h3 id="representables-and-yoneda-1">Representables and Yoneda 1</h3>
<p>http://youtu.be/4QgjKUzyrhM</p>
<ul>
<li>Definition of representable functors (co- or contravariant): those which are naturally isomorphic to $H^A$ or $H_A$ for some $A$.
<ul>
<li>Contravariant: $H_A : C^{\text{op}} \to \mathbf{Set}$; $H_A(X) = C(X,A)$.</li>
<li>Covariant: $H^A : C \to \mathbf{Set}$; $H^A(X) = C(A,X)$.</li>
</ul></li>
<li>Is there a functor $A \to H_A$? Yes, the <em>Yoneda embedding</em> $Y$:
<ul>
<li>$Y(A) = H_A$</li>
<li>$Y(f) = H_f$, where $H_f : H_A \to H_B$ is postcomposition with $f$. $H_f$ is a natural transformation; its component at $X$ has type $H_f(X) : H_A(X) \to H_B(X) = C(X,A) \to  C(X,B)$. Postcomposing an arrow in $C(X,A)$ with $f : A  \to B$ yields an arrow in $C(X,B)$.</li>
</ul></li>
</ul>
<h3 id="representables-and-yoneda-2">Representables and Yoneda 2</h3>
<p>http://youtu.be/eaJmUUogb6g</p>
<ul>
<li>Proof that Yoneda embedding $C \to [C^{\text{op}}, \mathbf{Set}]$ sends morphisms $f$ to <em>natural transformations</em> $H_f$. Comes down to the fact that composition in the category is associative.</li>
</ul>
<h3 id="representables-and-yoneda-3">Representables and Yoneda 3</h3>
<p>http://youtu.be/TLMxHB19khE</p>
<ul>
<li>Look at natural transformations from $H_A$ to some other (contravariant) functor $F$. Big idea: such natural transformations $\alpha$ are entirely determined by where $\alpha_A$ sends $1_A$.</li>
<li>Yoneda lemma: $F\ A \cong [C^{\text{op}}, \mathbf{Set}](H_A, F)$ (natural in $A$ and $F$). <em>I.e.</em> the set of objects in $F\ A$ is isomorphic to the hom-set of natural transformations between $H_A$ and $F$.</li>
</ul>
<h2 id="adjunctions-part-1">Adjunctions (part 1)</h2>
<h3 id="adjunctions-1">Adjunctions 1</h3>
<p>http://youtu.be/loOJxIOmShE</p>
<ul>
<li>Given categories $C$ and $D$ and functors $F : C \to D$ and $G : D  \to C$, we have the following situations:
<ul>
<li>Isomorphism: $1 = GF$, $FG = 1$</li>
<li>Equivalence: $1 \cong GF$, $FG \cong 1$</li>
<li>Adjunction: $\eta : 1 \Rightarrow GF$, $\varepsilon : FG \Rightarrow 1$ So we can think of an adjunction as a “weaker sort of equivalence”.</li>
</ul></li>
<li>$\eta$ and $\varepsilon$ are subject to triangle identities: $F  \stackrel{F\eta}{\Longrightarrow} FGF \stackrel{\varepsilon  F}{\Longrightarrow} F$ is the identity, and similarly for $\eta G ; G \varepsilon$.</li>
<li>These laws can be expressed as commuting diagrams of 2-cells: draw $\eta$ and $\varepsilon$ as 2-cells and paste them in two different ways.</li>
</ul>
<h3 id="adjunctions-2">Adjunctions 2</h3>
<p>http://youtu.be/JEJim3t-N9A</p>
<ul>
<li>Alternate definition of adjunction $F \dashv G$: an isomorphism $D(F X, Y) \cong C(X, G Y)$ natural in $X$ and $Y$.</li>
<li>What “natural in $X$ and $Y$” means here.</li>
<li>Hint: sending identity morphisms across the iso gives us $\eta$ and $\varepsilon$ from the first definition. Proof deferred to Adjunctions 4.</li>
</ul>
<h3 id="adjunctions-4">Adjunctions 4</h3>
<p>http://youtu.be/nP5XQ6OBHHY</p>
<ul>
<li>Note: Adjunctions 4, not 3, follows on to 2.</li>
<li>Given: an isomorphism $D(FX,Y) \cong C(X,GY)$ which is natural in $X$ and $Y$.</li>
<li>Notation: write application of the isomorphism as an overbar.</li>
<li>Construct the two squares implied by naturality. Follow them each around in <em>both</em> directions (since they involve a natural <em>isomorphism</em>) to get four equations in total governing how the iso interacts.</li>
<li>Define $\eta$ and $\varepsilon$ by applying the isomorphism to appropriate identity morphisms. Naturality and the triangle identities follow from the above four equations.</li>
</ul>
<h2 id="monads">Monads</h2>
<h3 id="monads-1">Monads 1</h3>
<p>http://youtu.be/9fohXBj2UEI</p>
<ul>
<li>Monads give us a way to talk about algebraic theories (monoids, categories, groups, <em>etc.</em>).</li>
<li>Definition of a monad:
<ul>
<li>Functor $T : C \to C$</li>
<li>“unit” $\eta : 1 \Rightarrow T$</li>
<li>“multiplication” $\mu : T^2 \Rightarrow T$</li>
<li>with unit and associativity laws.</li>
</ul></li>
<li>Note what is meant by a commutative diagram of natural transformations</li>
<li>Example: monad for monoids (aka the list monad)
<ul>
<li>$C = \mathbf{Set}$</li>
<li>$T = X \mapsto X^*$, maps a set $X$ to set of words in $X$ (<em>i.e.</em> lists)</li>
<li>$\eta$ is singleton</li>
<li>$\mu$ is concatenation</li>
<li>Note, unit and associativity for <em>monad</em> is different than unit and associativity of <em>monoids</em>, which has already been encoded in the definition of $T$.</li>
</ul></li>
</ul>
<h3 id="monads-2">Monads 2</h3>
<p>http://youtu.be/Si6_oG7ZdK4</p>
<ul>
<li>Proof that the list monad (“monad for monoids”) is in fact a monad</li>
<li>Example: monad for small categories
<ul>
<li>$C = \mathbf{Gph}$, category of graphs</li>
<li>$T$ makes the free category on a graph (morphisms = paths in the underlying graph)</li>
<li>With only one object, this reduces to the monad for monoids.</li>
<li>Proof of monads laws is basically the same as for the list monad.</li>
</ul></li>
</ul>
<h3 id="monads-3">Monads 3</h3>
<p>http://youtu.be/eBQnysX7oLI</p>
<ul>
<li>Algebras for monads. Monads are supposed to be like algebraic theories; algebras are models.</li>
<li>An <em>algebra</em> for a monad $(T : C \to C, \eta, \mu)$ is an object $A  \in C$ (the “underlying object”) equipped with an “action” $\theta :  T\ A \to A$, satisfying the “obvious” axioms ($\theta$ must interact “sensibly” with $\eta$ and $\mu$).
<ul>
<li>$\eta ; \theta = 1$</li>
<li>$\mu ; \theta = T \theta ; \theta$</li>
</ul></li>
<li>Example: $C = \mathbf{Set}$, $T$ = list monad (“monad for monoids”)
<ul>
<li>An algebra is a set $A$ equipped with $\theta : [A] \to A$</li>
<li>First axiom says $\theta$ must simply project element out of length-one list.</li>
<li>Other axiom is essentially associativity.</li>
<li>That is, algebras for the list monad are monoids.</li>
</ul></li>
<li>Example for monad of categories (from last time) works the same way.</li>
</ul>
<h3 id="monads-3a">Monads 3A</h3>
<p>http://youtu.be/uYY5c1kkoIo</p>
<p>More on monoids as monad algebras of the list monad.</p>
<ul>
<li>Given a monad algebra $\theta : [A] \to A$, construct the monoid:
<ul>
<li>whose underlying set is $A$</li>
<li>$a \cdot b = \theta [a,b]$</li>
<li>$\varepsilon = \theta []$.</li>
</ul></li>
<li>The monad algebra law for $\eta$ (a triangle) just says that $\theta$ can’t do anything interesting on one-element lists: it has to just return the single element.</li>
<li>Identity and associativity laws for the monoid come from the other monad algebra law, saying how $\theta$ interacts with $\mu$ (a square), and from how the list functor is defined. We <em>start with</em> a way of mapping <em>lists</em> down to values, which bakes in the idea that it doesn’t matter how we associate the list.</li>
</ul>
<h3 id="monads-4">Monads 4</h3>
<p>http://youtu.be/Cm-O_ZWEIGY</p>
<p>Monad algebras form a category (called $\mathbf{Alg}\ T$).</p>
<ul>
<li><p>Given two monad algebras $\theta : T A \to A$ and $\varphi : T B \to  B$, a morphism between them consists of a morphism of underlying objects, $f : A \to B$, such that the obvious square commutes.</p></li>
<li><p>Example. List monad again. $C = \mathbf{Set}, T = []$. A morphism of monoids is a function $f : A \to B$ such that $f (xy) =  f(x)f(y)$. See how this equation arises from the commuting square for monad morphisms, by starting with a 2-element list in upper left and following it around.</p></li>
<li><p>Given a particular mathematical theory, can it be expressed as the category of algebras for some monad? <em>I.e.</em> given a category $D$, is it equivalent to $\mathbf{Alg}\ T$ for some $T$? (Answer: no, not in general, <em>e.g.</em> category of topological spaces can’t.)</p></li>
<li><p>But this is still an interesting question, more or less the question of “monadicity”. Category $D$ said to be <em>monadic</em> over category $C$ if $D$ can be expressed as category of algebras of monads over $C$.</p></li>
</ul>
<h2 id="adjunctions-and-monads">Adjunctions and monads</h2>
<h3 id="adjunctions-3">Adjunctions 3</h3>
<p>http://youtu.be/2i_PpYsl8b8</p>
<ul>
<li>Note: depends on monads.</li>
<li>Examples of adjunctions:
<ul>
<li>between the category of sets and the category of monoids: $\text{Free} \dashv \text{Forget}$</li>
<li>similarly between category of graphs and category $\mathbf{Cat}$ of (small) categories.</li>
</ul>
<p>In general, free functors are left adjoint to forgetful functors. (How to remember the direction: “left” has four letters, just like “free”.)</p></li>
<li>Every adjunction $(F \dashv G : C \to D, \eta, \varepsilon)$ gives rise to a monad $(GF, \eta, \mu = G \varepsilon F)$. Check monad laws:
<ul>
<li>Monad triangle laws are just adjunction triangle laws with extra $G$ or $F$ everywhere.</li>
<li>Monad associativity law is naturalty for $\varepsilon$, or something like that.</li>
</ul></li>
</ul>
<h3 id="adjunctions-5">Adjunctions 5</h3>
<p>http://youtu.be/xqLgGB7Hv7g</p>
<p>“Every monad comes from an adjunction via its category of algebras.”</p>
<p>Last time we showed every adjunction gives rise to a monad. What about the converse?</p>
<p>Answer: yes. In fact, given a monad, there is an entire category of adjunctions which give rise to it, which always has initial and terminal objects: these are the constructions found by Kleisli and by Eilenberg-Moore, respectively. Intuitively, any other adjunction giving rise to the monad can be described by the morphisms between it and the Kleisli and Eilenberg-Moore constructions.</p>
<p>Let $(T : C \to C, \eta, \mu)$ be a monad.</p>
<ul>
<li><p>Terminal solution (Eilenberg-Moore): consider category $\mathbf{Alg}\ T$ of $T$-algebras, also written $C^T$. We construct an adjunction $F \dashv G : C \to C^T$. (Intuition: $F : C \to C^T$ “freely” constructs a $T$-algebra; $G : C^T \to C$ “forgets” the algebra structure.)</p>
<ul>
<li><p>$G : C^T \to C$ is easy to construct: $(A, \theta : T A \to A)  \mapsto A$.</p></li>
<li><p>What about $F : C \to C^T$? Sends $A$ to the “free” $T$-algebra on $A$, with underlying set $T A$. Then evaluation map is $\mu$. That is, $F A = (T A, \mu : T (T A) \to T A)$. Need to check that this definition of $F$ really gives a monad algebra as a result. In this case the monad algebra laws are just the monad laws for $T$!</p></li>
<li><p>Now define a unit and counit. $\eta_A : A \to GFA = A \to TA$ is just the $\eta$ for the monad. $\varepsilon_\theta :  FG(A,\theta) \to (A,\theta)$ is an algebra morphism from the free algebra on $A$ (<em>i.e.</em> $(TA, \mu)$) to $(A,\theta)$: in fact, $\theta$ itself is such a morphism, by the second algebra law.</p></li>
<li><p>Prove triangle laws for $\eta$ and $\varepsilon$: exercise for the watcher/reader.</p></li>
</ul></li>
</ul>
<h3 id="adjunctions-6">Adjunctions 6</h3>
<p>http://youtu.be/Ht1mQ97Zq2k</p>
<p>This time, initial solution to “does a monad give rise to any adjunctions”: Kleisli.</p>
<ul>
<li>The Kleisli category for a monad $T$ on category $C$, written $\mathbf{Kl}(T)$ or $C_T$
<ul>
<li>Objects: objects of $C$.</li>
<li>Morphisms: $C_T(A,B) :\equiv C(A,T B)$.</li>
<li>Composition: given $f : A \to T B$ and $g : B \to T C$, produce $f ; T g ; \mu : A \to T C$.</li>
<li>Identity: $\eta : A \to T A$.</li>
<li>Category axioms come from monad axioms. Associativity comes from associativity and naturality of $\mu$; unit laws come from unit laws for $\eta$.</li>
</ul></li>
<li><p>Intuition: this is the category of free algebras: $A \to T B = GFB$ is equivalent, under the adjunction, to $F A \to F B$, morphism between free algebras.</p></li>
<li><p>Note, for the Eilenberg-Moore category (last time) it was complicated to define the objects and simple to define the morphisms. For Kleisli, it’s the other way around. “Conservation of complicatedness.”</p></li>
</ul>
<h3 id="adjunctions-7">Adjunctions 7</h3>
<p>http://youtu.be/D8g9xnVr0Lg</p>
<p>The adjunction that comes from the Kleisli category, giving rise to the original monad $T$.</p>
<p>Again, let $(T : C \to C, \eta, \mu)$ be a monad. We will construct $F_T \dashv G_T : C \to C_T$, where $C_T$ is the Kleisli category defined in Adjunctions 6, with $G_T F_T = T$.</p>
<ul>
<li>$F_T : C \to C_T$ sends objects to “free algebras”
<ul>
<li>Identity on objects.</li>
<li>On morphisms, sends $f : A \to B$ to $\eta ; T f : A \to T B$ (equivalently $f ; \eta$).</li>
</ul></li>
<li>$G_T : C_T \to C$ sends a “free algebra” to its “underlying object”
<ul>
<li>Sends $A$ to $T A$.</li>
<li>Sends $f : A \to T B$ to $T f ; \mu : T A \to T B$.</li>
</ul></li>
<li>Unit and counit
<ul>
<li>$\eta : A \to G_T F_T A = T A$ we can take as the $\eta$ of the monad.</li>
<li>$\varepsilon : F_T G_T A = T A \to T A$ we can take to be id.</li>
</ul></li>
<li>Adjunction laws come down to monad laws (left to viewer).</li>
</ul>
<p>Given a monad $T$ on $C$, we have a category of adjunctions $\mathbf{Adj}(T)$ giving rise to $T$ (morphisms are functors making everything commute). $C_T$ is the initial object and $C^T$ is terminal.</p>
<p>Question of <em>monadicity</em>: given an adjunction $F \dashv G$, is $D \cong C^T$? If so, say “$D$ is monadic over $C$”, <em>i.e.</em> everything in $D$ can be expressed as monad algebras of $C$. Or can say the adjunction is a “monadic adjunction”. Can also say that the right adjoint (forgetful functor $G$) “is monadic”. Monadic adjunctions are particularly nice/canonical.</p>
<h2 id="string-diagrams">String diagrams</h2>
<h3 id="string-diagrams-1">String diagrams 1</h3>
<p>http://youtu.be/USYRDDZ9yEc</p>
<p>Way of notating natural transformations and functors. Poincare dual: 0D things (points, i.e. categories) become 2D (regions), 1D things (lines, i.e. functors) stay 1D, 2D things (cells, i.e. natural transformations) become 0D.</p>
<p>String diagrams should be read right-left and bottom-top.</p>
<p>Horizontal and vertical composition of NTs correspond to horizontal and vertical juxtaposition of string diagrams.</p>
<p>Can leave out vertical lines corresponding to identity functor.</p>
<h3 id="string-diagrams-2">String diagrams 2</h3>
<p>http://youtu.be/JeGhNhgOTuk</p>
<p>Recall the interchange law, which says that vertical and horizontal composition of natural transformations commute. This guarantees that string diagrams are well-defined, since the diagram doesn’t specify which happens first.</p>
<p>Whiskering is represented in string diagrams by horizontally adjoining a straight vertical line.</p>
<h3 id="string-diagrams-3">String diagrams 3</h3>
<p>http://youtu.be/pmvVE8AGAEA</p>
<p>Given an adjunction $F \dashv G$, we have natural transformations $\varepsilon : FG \to 1$ and $\eta : 1 \to GF$, and two laws given by triangles. What do these look like as string diagrams? $\varepsilon$ is a cap, $\eta$ a cup, and the triangle laws look like pulling wiggly strings straight!</p>
<h3 id="string-diagrams-4">String diagrams 4</h3>
<p>http://youtu.be/YNC5faXshAk</p>
<p>Monads in string diagrams. Draw $\mu$, $\eta$, and the monad laws as nice-looking string diagrams with nice topological intuition.</p>
<h3 id="string-diagrams-5">String diagrams 5</h3>
<p>http://youtu.be/kiXjcqxVogE</p>
<p>Seeing how monads arise from adjunctions, using string diagrams.</p>
<h2 id="pipe-cleaners">Pipe cleaners</h2>
<p>These are presented without any commentary or explanation that I can find. Each of the below videos just presents a 3D structure made out of pipe cleaners with no explanation. Maybe there is some other catsters video that presents a motivation or explanation for these; if I find it I will update the notes here. I can see that it might have something to do with string diagrams, and that you can make categories out of these sorts of topological structures (<em>e.g.</em> with gluing as composition) but otherwise I have no clue what this is about.</p>
<ul>
<li>Open-closed cobordisms 1: http://youtu.be/Jb1ZHLXBMy4</li>
<li>Open-closed cobordisms 2 (“zig-zag-ator”): http://youtu.be/zQMhXy1-YNM</li>
<li>Open-closed cobordisms 3 (“cut-off pair of pants”): http://youtu.be/_raQJYpEnU8</li>
</ul>
<p>There is also:</p>
<ul>
<li>Klein bottle: https://www.youtube.com/watch?v=kteH2ZBW9Lg</li>
</ul>
<p>This is a nice 5-minute presentation about Klein bottles, complete with pipe cleaner model. Though it seems to have little to do with category theory.</p>
<p>Also also:</p>
<ul>
<li>Outtakes: https://www.youtube.com/watch?v=guVjFOfi2o0</li>
</ul>
<p>This has nothing to do with either pipe cleaners or category theory, but it is midly amusing.</p>
<h2 id="general-limits-and-colimits">General Limits and Colimits</h2>
<h3 id="general-limits-and-colimits-1">General limits and colimits 1</h3>
<p>http://www.youtube.com/watch?v=g47V6qxKQNU</p>
<p>Defining limits in general, informally.</p>
<ul>
<li>The thing we take a limit <em>of</em> is called a <em>diagram</em> (a collection of objects and morphisms). A limit of a diagram is a <em>universal cone</em>.</li>
<li>A <em>cone</em> over a diagram is an object (<em>vertex</em>) together with morphisms (<em>projection maps</em>) to all objects in the diagram, such that all triangles commute.</li>
<li>Universal cone is the “best” one, through which all other cones factor, <em>i.e.</em> there is a unique morphism from the vertex of one to the other such that all the relevant triangles commute.</li>
</ul>
<h3 id="general-limits-and-colimits-2">General limits and colimits 2</h3>
<p>http://www.youtube.com/watch?v=SpCyaNi257w</p>
<p>Examples of limits.</p>
<ul>
<li>Terminal objects: limit over the empty diagram.</li>
<li>Products: limit over discrete diagram on two objects.</li>
<li>Pullback: limit over a “cospan”, <em>i.e.</em> a diagram like $X \to Y  \leftarrow Z$. Note that we usually ignore the edge of the cone to $Y$, since it is uniquely determined by the edges to $X$ and $Z$.</li>
<li>Equalizer: limit over a parallel pair of arrows.</li>
</ul>
<h3 id="general-limits-and-colimits-3">General limits and colimits 3</h3>
<p>http://www.youtube.com/watch?v=U3nzEUEnLKQ</p>
<ul>
<li>Note: requires natural transformations.</li>
<li>Formal definitions of:
<ul>
<li>Diagram (functor from an index category)</li>
<li>Cone (natural transformation from constant functor to diagram).</li>
</ul></li>
</ul>
<h3 id="general-limits-and-colimits-4">General limits and colimits 4</h3>
<p>http://youtu.be/7B4cawdLAPg</p>
<ul>
<li>Requires Yoneda.</li>
<li><p>Formal definition of a limit: given a diagram $D : I \to C$, a limit for $D$ is an object $U$ together with a family of isomorphisms $C(V,U) \cong [I,C](\Delta_V, D)$ natural in $V$. <em>I.e.</em> a natural correspondence between morphisms $V \to U$ (the “factorization” from one cone to another) and morphisms (<em>i.e.</em> natural transformations) from $\Delta_V$ to $D$ in the functor category $[I,C]$ (<em>i.e.</em> cones over $D$ with vertex $V$). That is, every cone with vertex $V$ has a unique factorization morphism, <em>and vice versa</em>!. The “vice versa” part is the surprising bit. If we have a limit then <em>every</em> morphism is the factorization for some cone to the universal cone.</p></li>
<li><p>If we set $V = U$ then $C(U,U) \cong \dots$ <em>etc.</em> In particular $1_U :  C(U,U)$ corresponds to some cone, which is THE universal cone. The Yoneda lemma says (?) that the entire natural isomorphism is determined by this one piece of data (where $1_U$ goes). Note that both $C(-,U)$ and $[I,C](\Delta_-, D)$ are functors $C^{\text{op}} \to \mathbf{Set}$. The Yoneda lemma says that a natural transformation from $C(-,U) = H_U$ to $[I,C](\Delta_-, D)$ is isomorphic to $[I,C](\Delta_U, D)$ — i.e. a cone with vertex $U$, the universal cone.</p></li>
<li><p>The universality of this cone apparently comes from naturality.</p></li>
</ul>
<h3 id="general-limits-and-colimits-5">General limits and colimits 5</h3>
<p>http://youtu.be/Ud_k4HFIogQ</p>
<ul>
<li>Requires adjunctions.</li>
<li>Notation for limits. Categories that “have all limits (of a given shape)”.</li>
<li>The natural isomorphism defining a limit can be seen as an adjunction $\Delta \dashv \lim_{I \leftarrow}$ where $\Delta : C \to [I,C]$, and $\lim_{I \leftarrow} : [I,C] \to C$ is the functor that takes a diagram and produces its limit.</li>
<li>Claim: this is an adjunction <em>if</em> $C$ has all $I$-limits. Need to show that the iso is also natural in $D$, and that $\lim_{I  \leftarrow}$ is actually a functor.</li>
</ul>
<h3 id="general-limits-and-colimits-6">General limits and colimits 6</h3>
<p>http://youtu.be/9UOdrRF_pNc</p>
<p>Colimits using the same general formulation. “Just dualize everything”.</p>
<ul>
<li><p>Cocone (“cone under the diagram”) is an object with morphisms <em>from</em> the objects in the diagram such that everything commutes.</p></li>
<li><p>Universal cocone: for any other cocone, there is a unique morphism from the universal cocone <em>to</em> the other cone which makes everything commute. Note it has to go that direction since the universal cocone is supposed to be a “factor” of other cocones.</p></li>
<li><p>In Eugenia’s opinion the word “cocone” is stupid.</p></li>
<li><p>More generally: natural isomorphism between cocones and morphisms. $C(\lim_{\to I}, V) \cong [I,C](D, \Delta_V)$. Limits in $C$ are the same as colimits in $C^{op}$, and vice versa.</p></li>
<li><p>All limits are terminal objects in a category of cones (and colimits are initial objects).</p></li>
<li><p>Since terminal objects are initial objects in $C^{op}$ (and vice versa), we can even say that all universal properties are initial objects (and terminal objects) <em>somewhere</em>.</p></li>
</ul>
<h2 id="slice-and-comma-categories">Slice and comma categories</h2>
<h3 id="slice-and-comma-categories-1">Slice and comma categories 1</h3>
<p>https://www.youtube.com/watch?v=f4jpvwwnq_s</p>
<p>Slice category. Given a category $C$, fix an object $X \in C$. Then we define the slice category $C/X$ by</p>
<ul>
<li>Objects are pairs $(A,p)$ where $p : A \to X$.</li>
<li>Morphisms from $(A,p)$ to $(B,q)$ are morphisms $h : A \to B$ in $C$ which make the triangle commute.</li>
</ul>
<p>Coslice category, or “slice under” category $X/C$ is the dual of $C/X$, i.e. objects are pairs $(A,p)$ where $p : X \to A$, etc.</p>
<ul>
<li><p>If $C$ has a terminal object $1$, $C/1 \cong C$. (Dually, $0/C \cong C$.)</p></li>
<li><p>Products in $C/X$ are pullbacks in $C$ having $X$ as a corner. (Dually, coproucts in $X/C$ are pushouts.)</p></li>
</ul>
<h3 id="slice-and-comma-categories-2">Slice and comma categories 2</h3>
<p>http://youtu.be/W6sG5uraex0</p>
<p>Comma categories are a generalization of slice categories. Fix a functor $F : C \to D$ and an object $X \in D$. Then we can form the comma category $F \downarrow X$.</p>
<ul>
<li>Objects: pairs $(c \in C, p : F c \to X)$. Image of some object under $F$ and an arrow from it to $X$.</li>
<li>Morphisms are morphisms $f$ in $C$ such that $F f$ makes the relevant diagram commute.</li>
</ul>
<p>Of course we can dualize, $X \downarrow F$ (“cocomma” sounds even stupider than “cocone”, perhaps).</p>
<p>Apparently comma categories give us nice ways to talk about adjunctions.</p>
<p>Let’s generalize even more! Fix the functor $F$ but not the object $X \in D$. Then we can form $F \downarrow D$:</p>
<ul>
<li>Objects: triple $(c \in C, X \in D, p : F C \to X)$.</li>
<li>Morphism $(c, X, p) \to (c', X', p')$ is a pair of morphisms $c \to  c'$ and $X \to X'$ such that the relevant square commutes.</li>
</ul>
<p>Can also dualize, $D \downarrow F$.</p>
<p>An <em>even further</em> generalization! Start with two functors $F : C \to D$, $G : E \to D$. Form $F \downarrow G$:</p>
<ul>
<li>Objects: triples $(c \in C, e \in E, p : F c \to G e)$.</li>
<li>Morphisms: obvious generalization.</li>
</ul>
<p>In fact, all of these constructions are universal and can be seen as limits/colimits from the right point of view. “Next time”. (?)</p>
<h2 id="coequalisers">Coequalisers</h2>
<h3 id="coequalisers-1">Coequalisers 1</h3>
<p>https://youtu.be/vYktRTtulek</p>
<p>Coequalisers are a colimit. Show up all over the place. Give us quotients and equivalence relations. Also tell us about <em>monadicity</em> (given an adjunction, is it a monadic one?).</p>
<p>Definition: a <em>coequaliser</em> is a colimit of a diagram consisting of two parallel arrows.</p>
<p>More specifically, given $f,g : A \to B$, a coequaliser is an object $C$ equipped with $e : B \to C$ such that $f;e = g;e$, with a universal property: given any other $s : B \to V$ with $f;s = g;s$, $s$ factors uniquely through $e$.</p>
<ul>
<li><p>Example: in $\mathbf{Set}$: coequaliser of $f,g : A \to B$ is a quotient $B/\sim$, where $\sim$ is the equivalence relation generated by $f(a) \sim g(a)$ for all $a \in A$.</p></li>
<li><p>Conversely, we can <em>start</em> with an equivalence relation and build it using a coequaliser. Given: an equivalence relation $R \subseteq B  \times B$. Note we have $\pi_1, \pi_2 : R \to B$. Coequaliser is equivalence classes of $R$.</p></li>
</ul>
<h3 id="coequalisers-2">Coequalisers 2</h3>
<p>https://www.youtube.com/watch?v=DMSPS6us__Y</p>
<p>Quotient groups as coequalisers. Consider a group $G$ and a normal subgroup $H \triangleleft G$. In the category of groups, consider two parallel maps $H \to G$: the inclusion map $\iota$, and the zero map $0$ which sends everything to the identity element $e \in G$. Claim: the coequaliser of these two maps is the quotient group $G/H$, together with the quotient map $G \to G/H$.</p>
<p>Let’s see why. Suppose we have another group $V$ with a group homomorphism $\theta : G \to V$ such that $\iota ; \theta = 0 ; \theta = 0$; that is, $\theta(h) = e$ for all $h \in H$. We must show there is a unique homomorphism $G/H \to V$ which makes the diagram commute.</p>
<p>Notation: $g \in G$ under the quotient map gets sent to $[g] = gH$ ($g_1 \sim g_2$ iff $g_2 g_1^{-1} \in H$). For the homomorphism $G/H \to V$, send $[g]$ to $\theta(g)$. Note this is required to make things commute, which gives us uniqueness; we must check this is well-defined and a group homomorphism. If $g_1 \sim g_2$ then $g_2 g_1^{-1} \in H$. By definition, $\theta(g_2 g_1^{-1}) = e$, and since $\theta$ is a group homomorphism, $\theta(g_2) = \theta(g_1)$. Hence it is well-defined, and must additionally be a group homomorphism since $[g_1] [g_2] = [g_1 g_2]$ and $\theta$ is a group homomorphism.</p>
<h2 id="monoid-objects">Monoid objects</h2>
<h3 id="monoid-objects-1">Monoid objects 1</h3>
<p>https://www.youtube.com/watch?v=PH-OhkrXXvA</p>
<p>Idea: take the definition of monoids from $\mathbf{Set}$, and “plunk it” into any other category with enough structure.</p>
<ul>
<li>A <em>monoid</em> is:
<ul>
<li>A set $A$</li>
<li>A binary operation $a \cdot b$ on $A$</li>
<li>A unit $e \in A$</li>
<li>Associativity: $\forall a,b,c \in A, (ab)c = a(bc)$</li>
<li>Identity: $\forall a, ea = a = ae$</li>
</ul></li>
</ul>
<p>Now let’s reexpress this categorically in $\mathbf{Set}$. Note we have been talking about <em>elements</em> of sets; we have to replace this with use of only objects and morphisms of $\mathbf{Set}$.</p>
<ul>
<li>A <em>monoid</em> (take 2) is:
<ul>
<li>An object $A \in \mathbf{Set}$</li>
<li>A morphism $\mu : A \times A \to A$ (note we use Cartesian product structure of $\mathbf{Set}$)</li>
<li>A morphism $\eta : 1 \to A$</li>
<li>A commutative diagram</li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/ouudfwc.png" />

</div>
</div>
<ul>
<li>A commutative diagram</li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/nbbqcho.png" />

</div>
</div></li>
</ul>
<p>Now we take the definition and port it to any monoidal category.</p>
<ul>
<li>A <em>monoid object</em> in a monoidal category $C$ is:
<ul>
<li>An object $A \in C$</li>
<li>A morphism $\mu : A \otimes A \to A$</li>
<li>A morphism $\eta : 1 \to A$</li>
<li>A commutative diagram</li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/nfy8o4h.png" />

</div>
</div>
<ul>
<li>A commutative diagram</li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/ntvfd67.png" />

</div>
</div></li>
</ul>
<h3 id="monoid-objects-2">Monoid objects 2</h3>
<p>https://www.youtube.com/watch?v=7Sf3Y4sesZE</p>
<p>Today: monoid object in the category of monoids is a commutative monoid.</p>
<p>Note first the category of monoids is itself monoidal under Cartesian product. That is, given two monoids $M$ and $N$, $M \times N$ is also a monoid.</p>
<p>Now, what is a monoid object in $\mathbf{Mon}$?</p>
<ul>
<li>An object $M \in \mathbf{Mon}$, <em>i.e.</em> a monoid</li>
<li>Monoid morphisms $\eta : 1 \to M$ and $\mu : M \times M \to M$</li>
<li>…satisfying unit and associativity laws.</li>
</ul>
<p>$\eta : 1 \to M$ is a <em>monoid</em> morphism so it has to send the single object of $1$ to the unit of $M$. Hence $\eta$ is entirely constrained and uninteresting.</p>
<p>$\mu : M \times M \to M$ has to be a monoid map. That is, $\mu((a,b) \circ (c,d)) = \mu(a,b) \circ \mu(c,d)$, <em>i.e.</em> $\mu(a \circ c, b \circ d) = \mu(a,b) \circ \mu(c,d)$. So $\mu$ has to “commute” with $\circ$. This is precisely the condition needed to apply Eckmann-Hilton.</p>
<p>Monoid object is also required to satisfy unital and associativity laws, but we can already deduce those from Eckmann-Hilton.</p>
<h2 id="categories">2-categories</h2>
<h3 id="categories-1">2-categories 1</h3>
<p>https://www.youtube.com/watch?v=k-RehY4tLdI</p>
<p>Generalization of categories: not just objects and morphisms, but also (2-)morphisms between the (1-)morphisms. Primordial example: categories, functors, and natural transformations.</p>
<p>Note: today, <em>strict</em> 2-categories, i.e. everything will hold on the nose rather than up to isomorphism. A bit immoral of us. [If we let things be a bit looser we get bicategories?]</p>
<p>Recall: a (small) <em>category</em> $C$ is given by</p>
<ul>
<li>A set $\mathrm{Ob}\ C$ of objects</li>
<li>for all $x,y \in \mathrm{Ob}\ C$, a set $C(x,y)$ of morphisms</li>
</ul>
<p>equipped with</p>
<ul>
<li>identities: for all $x \in C$ a function $1 \to C(x,x)$</li>
<li>composition: for all $x,y,z \in C$, a composition function $C(y,z)  \times C(x,y) \to C(x,z)$.</li>
<li>unit and associativity laws.</li>
</ul>
<p>To make this into a 2-category, we take the <em>set</em> of morphisms and <em>categorify</em> it. That turns some of the above <em>functions</em> into <em>functors</em>. Thus, a $2$-category $C$ is given by a set of objects along with</p>
<ul>
<li>a <em>category</em> $C(x,y)$ for each $x,y \in C$</li>
<li>a <em>functor</em> $1 \to C(x,x)$ for each $x \in C$</li>
<li>a composition <em>functor</em> $C(y,z) \times C(x,y) \to C(x,z)$.</li>
<li>etc.</li>
</ul>
<p>(Note: why not turn the set of <em>objects</em> into a category? That’s a good question. Turns out we would get something different.)</p>
<p>Let’s unravel this a bit. If $C(x,y)$ is a <em>category</em> then the objects are morphisms (of $C$) $x \to y$, and there can also be morphisms (of $C(x,y)$) <em>between</em> these morphisms: $2$-cells. $2$-cells can be composed (“vertical” composition).</p>
<p>We also have the composition functor $C(y,z) \times C(x,y) \to C(x,z)$. On “objects” (which are $1$-cells in $C$) the action of this functor is just the usual composition of $1$-cells. On morphisms (i.e. $2$-cells), it gives us “horiztonal” composition.</p>
<p>Next time: how functoriality gives us the interchange law.</p>
<h3 id="categories-2">2-categories 2</h3>
<p>http://youtu.be/DRGh-HESyag</p>
<p>Interchange in a 2-category comes from functoriality of the composition functor. The key is to remain calm.</p>
<p>The functor is $C(y,z) \times C(x,y) \to C(x,z)$. On morphisms, it sends pairs of $2$-cells to a single $2$-cell, the horizontal composite. What does functoriality mean? It means if we have two (vertically!) composable pairs of $2$-cells; the functor on their composition (<em>i.e</em> doing vertical composition pointwise) is the same as applying the functor to each (<em>i.e.</em> first doing the horizontal compositions) and then composing (vertically).</p>
<h2 id="eckmann-hilton">Eckmann-Hilton</h2>
<h3 id="eckmann-hilton-1">Eckmann-Hilton 1</h3>
<p>https://www.youtube.com/watch?v=Rjdo-RWQVIY</p>
<p>NOTE: There seems to be no catsters video actually explaining what a “bicategory” is. <a href="https://ncatlab.org/nlab/show/bicategory">According to the nlab</a> it is a weaker version of a 2-category, where certain things are required to hold only up to coherent isomorphism rather than on the nose.</p>
<p>Eckmann-Hilton argument. Originally used to show all higher homotopy groups are Abelian. We can use it for other things, e.g.</p>
<ul>
<li>A bicategory with only one 0-cell and one 1-cell is a commutative monoid.</li>
<li>A monoid object in the monoidal category of monoids is a commutative monoid. (waaaat)</li>
</ul>
<p>Idea: given a set with two unital binary operations, they are exactly the same, and commutative — as long as the operations interact in a certain coherent way.</p>
<p>Given a set with two binary operations $\circ$ and $\star$, such that</p>
<ul>
<li>$\circ$ and $\star$ are unital, with the same unit;</li>
<li>one of them distributes over the other, i.e. $(a \star b) \circ (c  \star d) = (a \circ c) \star (b \circ d)$,</li>
</ul>
<p>then $\circ = \star$, and the operation is commutative.</p>
<p>Geometric intuition: $\circ$ and $\star$ could be vertical and horizontal composition of $2$-cells in a bicategory. Then distributivity is just the interchange law.</p>
<p>Proof: use the “Eckmann-Hilton clock”. See video for pictures. Given e.g. $a \circ b$, “rotate” $a$ and $b$ around each other by inserting units and using interchange law.</p>
<p>In fact, it is not necessary to require that the two units are the same: it is implied by the interchange law. Left as an exercise.</p>
<h3 id="eckmann-hilton-2">Eckmann-Hilton 2</h3>
<p>https://www.youtube.com/watch?v=wnRqo7UHa-k</p>
<p>This time, show the interchange law implies the units are the same and associativity.</p>
<p>Let $v$ be the vertical unit and $h$ the horizontal unit. Then $((v \circ h) \star (h \circ v)) = h \star h = h$ but also by interchange law it is equal to $((v \star h) \circ (h \star v)) = v \circ v = v$, hence $h = v$.</p>
<p>$(a \circ 1) \star (b \circ c) = a \star (b \circ c)$; interchange gives $(a \star b) \circ (1 \star c) = (a \star b) \circ c$. Since the two operations have to be the same, this gives associativity.</p>
<p>Example. A (small) $2$-category with only one $0$-cell and only one $1$-cell is in fact a commutative monoid. Underlying set is set of $2$-cells. Operation is either $\circ$ or $\star$, which by Eckmann-Hilton are the same and commutative.</p>
<p>Bicategory case is a bit more complicated, since horizontal composition is not strictly unital. A bicategory with only one $0$-cell is a monoidal category. A bicategory with only one $1$-cell is a commutative monoid.</p>
<h2 id="distributive-laws">Distributive laws</h2>
<h3 id="distributive-laws-1">Distributive laws 1</h3>
<p>https://www.youtube.com/watch?v=mw4IhOLhDwY</p>
<p>Monads represent algebraic structure; a distributive law says when two algebraic structures interact with each other in a coherent way. Motivating example: multiplication and addition in a ring.</p>
<p>Let $S$, $T$ be monads on a category $C$. A <em>distributive law</em> of $S$ over $T$ is a natural transformation $\lambda : ST \to TS$, satisfying the “obvious” axioms: $\lambda$ needs to interact properly with the monad structure of $S$ and $T$, that is:</p>
<ul>
<li>$\eta_S T ; \lambda = T \eta_S$</li>
<li><p>$S \lambda ; \lambda S ; T \mu_S = \mu_S T ; \lambda$</p></li>
<li>$S \eta_T ; \lambda = \eta_T S$</li>
<li><p>$\lambda T ; T \lambda ; \mu_T S = S \mu_T ; \lambda$</p></li>
</ul>
<p>Example: $C = \mathbf{Set}$. $S$ = free commutative monoid monad (“multiplication”), $T$ = free abelian group monad (“addition”). Define $\lambda_X : STX \to TSX$: $TX$ is formal sums of elements of $X$, like $(a + b + c)$; $S$ constructs formal products. So we have to send things like $(a + b)(c + d)$ to formal sums of formal products, $ac + bc + ad + bd$.</p>
<p>In fact we have constructed the free ring monad, $TS$.</p>
<p>If we start with a monoid and consider the free group on its underlying elements, we can define a product using distributivity; so the free group on a monoid is a group. Formally, the free group monad lifts to the category of monoids (?).</p>
<h3 id="distributive-laws-2">Distributive laws 2</h3>
<p>https://www.youtube.com/watch?v=TLgjH9Y8HOc</p>
<p>More abstract story behind our favorite example: combining a group and a monoid to get a ring.</p>
<p>Note: distributive law (at least in this example) is definitely non-invertible: you can turn a product of sums into a sum of products, but you can’t necessarily go in the other direction.</p>
<p>Main result: A distributive law $\lambda : ST \to TS$ is equivalent to a lift of $T$ to a monad $T'$ on $S$-$\mathbf{Alg}$. $TS$ becomes a monad, and $TS$-$\mathbf{Alg}$ is equivalent to $T'$-$\mathbf{Alg}$.</p>
<p>When is $TS$ a monad? We need $\mu : TSTS \to TS$; can do this if we have $\lambda : ST \to TS$, then use $\mu_T \mu_S$. The laws for a distributive law ensure that this satisfies the monad laws.</p>
<p>Distributive law is equivalent to a lift of $T$ to a monad on $S$-$\mathbf{Alg}$?</p>
<ul>
<li>An $S$-algebra looks like $\theta : SX \to X$; we want $T'$ to send this to another $S$-algebra, with carrier $TX$, i.e. some $\phi :  STX \to TX$. But we have $T \theta : TSX \to TX$; precomposing with $\lambda$ gives us what we want, and the distributive law axioms ensure that $T'$ is a monad on $S$-$\mathbf{Alg}$.</li>
</ul>
<p>$TS$-$\mathbf{Alg}$ is equivalent to $T'$-$\mathbf{Alg}$?</p>
<ul>
<li><p>Since $T'$ is a monad on $\mathbf{Alg} S$, a $T'$-algebra has <em>an $S$-algebra</em> as its underlying object. So given some $S$-algebra $\theta : SX \to X$, a $T$’-algebra on it is a morphism of $S$-algebras from $T'\theta$ to $\theta$, that is,</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/kzpuvz4.png" />

</div>
</div>
<p>This essentially says that an algebra for $T'$ is simultaneously an algebra for $S$ and an algebra for $T$ which interact properly via the distributivity law.</p></li>
<li><p>An algebra for $TS$ is $\psi : TSX \to X$. Clear that from a $T'$-algebra we get a $TS$ algebra. What about vice versa? Just precompose $\psi$ with $\eta_T$ to get an $S$-algebra, and with $T  \eta_S$ to get a $T$-algebra. A $TS$-algebra says how to evaluate e.g. multiplication and additions all mixed up; precomposing with $\eta$ picks out the stuff with just multiplication or just addition. Apparently one can prove that the algebras you get this way do indeed interact nicely.</p></li>
</ul>
<h3 id="distributive-laws-3-aka-monads-6">Distributive laws 3 (aka Monads 6)</h3>
<p>https://www.youtube.com/watch?v=g-SCYArh5RY</p>
<p>Recall that a monad is a functor together with some natural transformations; we can see this as a construction in the $2$-category of categories, functors, and natural transformations. We can carry out the same construction in any $2$-category $C$, giving monads <em>in</em> $C$.</p>
<p>Let $C$ be a $2$-category (<em>e.g.</em> $\mathbf{Cat}$). A monad <em>in</em> $C$ is given by</p>
<ul>
<li>a $0$-cell $X$</li>
<li>a $1$-cell $T : X \to X$</li>
<li>a pair of $2$-cells $\eta : 1 \to T$ and $\mu : T^2 \to T$</li>
</ul>
<p>satisfying the usual monad axioms.</p>
<p>In fact, we get an entire $2$-category of monads inside $C$!</p>
<p>What is a morphism of monads? A monad functor $(X_1, S_1, \eta_1, \mu_1) \to (X_2, S_2, \eta_2, \mu_2)$ (<em>i.e.</em> a $1$-cell in the $2$-category of monads in $C$) is given by</p>
<ul>
<li>a $1$-cell $u : X_1 \to X_2$</li>
<li>a $2$-cell $\lambda : S_2 u \to u S_1$ (Note, this is not backwards! This is what we will need to map algebras of the firs monad to algebras of the second.)</li>
</ul>
<p>satisfying the axioms:</p>
<ul>
<li>$\eta_{S_2} u ; \lambda = u \eta_{S_1}$</li>
<li>$S_2 \lambda ; \lambda S_1 ; u \mu_{S_1} = \mu_{S_2} ; \lambda$</li>
</ul>
<p>A monad transformation (<em>i.e.</em> a $2$-cell in the $2$-category of monads in $C$) is given by</p>
<ul>
<li>a $2$-cell $\sigma : u_1 \to u_2$, satisfying $\sigma S_2 ;  \lambda_2 = \lambda_1 ; S_1 \sigma$ (something like that, see pasting diagrams of $2$-cells in the video).</li>
</ul>
<h3 id="distributive-laws-4">Distributive laws 4</h3>
<p>https://www.youtube.com/watch?v=FZeoHPRoBVk</p>
<p>Distributive laws, even more formally!</p>
<p>Consider the $2$-category of monads $\mathbf{Mnd}(C)$ in an arbitrary $2$-category $C$; monads <em>in</em> $\mathbf{Mnd}(C)$ are distributive laws!</p>
<p>Recall that a monad in an arbitrary $2$-category is a $0$-cell equipped with an endo-$1$-cell and appropriate $2$-cells $\eta$ and $\mu$. In $\mathbf{Mnd}(C)$:</p>
<ul>
<li>A $0$-cell in $\mathbf{Mnd}(C)$, that is, a monad in $C$ $(X,S)$.</li>
<li>A $1$-cell $(X,S) \to (X,S)$, that is, a functor $T : X \to X$ and $\lambda : ST \to TS$.</li>
<li>$2$-cells $\eta$ and $\mu$. Can check that these turn $T$ into a monad.</li>
<li>Axioms on $\lambda$ give exactly what is needed to make it a distributive law.</li>
</ul>
<p>Summarizing more concisely/informally, a monad in $\mathbf{Mnd}(C)$ is</p>
<ul>
<li>A $0$-cell $X$</li>
<li>A pair of monads $S$,$T$</li>
<li>A distributive law $\lambda : ST \to TS$.</li>
</ul>
<p>Consider the map $C \mapsto \mathbf{Mnd}(C)$. This actually defines an endofunctor $\mathbf{Mnd}(-)$ on $2$-$\mathbf{Cat}$, the category of (strict) $2$-categories and (strict) $2$-functors. In fact, Street showed that $\mathbf{Mnd}(-)$ is a monad! The “monad monad”.</p>
<p>The multiplication has type $\mathbf{Mnd}(\mathbf{Mnd}(C)) \to \mathbf{Mnd}(C)$. Recall that objects in $\mathbf{Mnd}(\mathbf{Mnd}(C))$ are a pair of monads $S$,$T$ plus a distributive law. In fact, the distributive law is precisely what is needed to make $TS$ into a monad, which is the monad returned by the multiplication.</p>
<h2 id="group-objects-and-hopf-algebras">Group Objects and Hopf Algebras</h2>
<h3 id="group-objects-and-hopf-algebras-1">Group Objects and Hopf Algebras 1</h3>
<p>https://www.youtube.com/watch?v=p3kkm5dYH-w</p>
<p>Take the idea of a group and develop it categorically, first in the category of sets and then transport it into other categories (though it may not be completely obvious what properties of $\mathbf{Set}$ we are using).</p>
<p>A group is of course a set $G$ with an associative binary product, inverses, and an identity element. Let’s make this categorical: don’t want to talk about internal structure of $G$ but just about $G$ as an object in $\mathbf{Set}$.</p>
<p>So a group is:</p>
<ul>
<li>an object $G \in \mathbf{Set}$</li>
<li>a multiplication morphism $\mu : G \times G \to G$</li>
<li>an inverse morphism $\gamma : G \to G$</li>
<li>a unit morphism $\eta : 1 \to G$ (<em>i.e.</em> “universal element”)</li>
</ul>
<p>together with axioms expressed as commutative diagrams:</p>
<ul>
<li>$(id \times \mu) ; \mu = (\mu \times id) ; \mu$</li>
<li>$(\eta \times id) ; \mu = id = (id \times \eta) ; \mu$ (note to be pedantic we also need to use $\lambda : G \to 1 \times  G$ and $\rho : G \to G \times 1$)</li>
<li>$\Delta ; (id \times \gamma) ; \mu = \varepsilon ; \eta$</li>
</ul>
<p>where $\Delta : G \to G \times G$ is the diagonal map (note the fact that we are using $\Delta$ is the most interesting part; see forthcoming lectures) and $\varepsilon : G \to 1$ is the unique map to a terminal set.</p>
<h3 id="group-objects-and-hopf-algebras-2">Group Objects and Hopf Algebras 2</h3>
<p>https://www.youtube.com/watch?v=kJ2X_U7X5WA</p>
<p>Note just $\mu$ and $\eta$ together with axioms (forgetting about $\gamma$ and its axioms) is the definition of a monoidal category. Not surprising since a group is a monoid with inverses.</p>
<p>Recall $\Delta : G \to G \times G$. We get that for free from the fact that the monoid we are using is really the categorical product; $\Delta$ can be easily defined using the universal property of categorical product.</p>
<p>In fact, every set $S$ is a <em>comonoid</em> in a unique way, since $\times$ is a categorical product. That is, a comonoid on a set $S$ is given by</p>
<ul>
<li>a comultiplication $\Delta : S \to S \times S$</li>
<li>a counit $\varepsilon : S \to 1$</li>
<li>satisfying coassociativity and counit axioms.</li>
</ul>
<p>And note we used $\Delta$ and $\varepsilon$ in the definition of a group, in particular in the axioms for $\gamma$.</p>
<h3 id="group-objects-and-hopf-algebras-3">Group Objects and Hopf Algebras 3</h3>
<p>https://www.youtube.com/watch?v=wAeHrtKMTHM</p>
<p>The definition given last time won’t work in general for any monoidal category, but it does work for any <em>Cartesian</em> category (that is, monoidal categories where the monoidal operation is categorical product). Examples of Cartesian categories, in which it therefore makes sense to have group objects, include:</p>
<ul>
<li>$\mathbf{Set}$</li>
<li>$\mathbf{Top}$ (category of topological spaces, with Cartesian product toplogy)</li>
<li>$\mathbf{Diff}$ (cat. of smooth manifolds?)</li>
<li>$\mathbf{Grp}$ (groups)</li>
<li>$\mathbf{Cat}$ (categories)</li>
</ul>
<p>Let’s see what a group object looks like in each of these examples.</p>
<ul>
<li>In $\mathbf{Set}$, a group object is a group.</li>
<li>In $\mathbf{Top}$, a topological group.</li>
<li>In $\mathbf{Diff}$, a Lie group.</li>
<li>In $\mathbf{Grp}$, it turns out a group object is an Abelian group! (Details left as an exercise.)</li>
<li>In $\mathbf{Cat}$, we get a “crossed module”.</li>
</ul>
<p>What about non-Cartesian monoidal categories? Simplest example is $\mathbf{Vect}$, category of (finite-dimensional) vector spaces with linear maps. Monoidal structure given by tensor product and complex numbers. Tensor product defined by</p>
<p>$V \otimes W = \{ \sum_t \alpha_t (v_t \otimes w_t) \mid v_t \in V, w\_t \in W \} / [(\alpha_1 v_1 + \alpha_2 v_2) \otimes w \sim \alpha_1 (v_1 \otimes w) + \alpha_2(v_2 \otimes w) \text{ and symmetrically}]$</p>
<p>Suppose $\{v_i\}$ is a basis for $V$ and $\{w_j\}$ is a basis for $W$, then $\{v_i \otimes w_j\}$ is a basis for $V \otimes W$.</p>
<p>The point is that $\mathrm{dim}(V \otimes W) = \mathrm{dim}(V) \times \mathrm{dim}(W)$, but that’s different than $\mathrm{dim}(V \times W) = \mathrm{dim}(V) + \mathrm{dim}(W)$, so $\mathbf{Vect}$ is <em>not</em> Cartesian.</p>
<h3 id="group-objects-and-hopf-algebras-4">Group Objects and Hopf Algebras 4</h3>
<p>https://www.youtube.com/watch?v=zZn9ZETVkF8</p>
<p>We still want to be able to define group objects in monoidal categories which are not Cartesian.</p>
<p>Recall: if we have a monoidal category $(C, \times, 1)$ where $\times$ is the categorical product, then every object $X \in C$ is a comonoid $(X, \Delta, \varepsilon)$ in a unique way, and every morphism is a comonoid map.</p>
<p>Notation: in $\mathbf{Set}$, an object with an associative binary operation and an identity is called a <em>monoid</em>; in $\mathbf{Vec}$ it’s called an <em>algebra</em>. So when we generalize to arbitrary categories sometimes “monoid” is used, sometimes “algebra”.</p>
<p>A <em>Hopf algebra</em> is a group object in a general monoidal (tensor) category. Details next time.</p>
<h3 id="group-objects-and-hopf-algebras-5">Group Objects and Hopf Algebras 5</h3>
<p>https://www.youtube.com/watch?v=gmxZ_KCRZho</p>
<p>A <em>Hopf algebra</em> $H$ in a (braided) monoidal category is as follows. We don’t get comonoid stuff for free any more so we have to put it in “by hand”.</p>
<ul>
<li>comonoid $\Delta : H \to H \otimes H$ and $\varepsilon : H \to 1$</li>
<li>monoid $\mu : H \otimes H \to H$ and $\eta : 1 \to H$</li>
<li>“antipode” or inverse $\gamma : H \to H$</li>
</ul>
<p>(See video for string diagrams.) Note the monoid and comonoid also need to be “compatible”: this is where the braidedness comes in. In particular $\mu$ and $\eta$ need to be <em>comonoid morphisms</em>. So we need $H \otimes H$ to be a coalgebra.</p>
<p>Lemma: suppose $H$, $K$ are comonoids. Then $H \otimes K$ is a coalgebra if the category is braided: $H \otimes K \to (H \otimes H) \otimes (K \otimes K)$ using comonoid structures on $H$ and $K$, and then using (associativity and) braiding we can flip inner $H \otimes K$ around to get $(H \otimes K) \otimes (H \otimes K)$.</p>
<p>Can then write down what it means for $\mu$ to be a coalgebra map aka comonoid morphism; left as an exercise (or the next video).</p>
<h3 id="group-objects-and-hopf-algebras-6">Group Objects and Hopf Algebras 6</h3>
<p>https://www.youtube.com/watch?v=Gv1sRLOwVWA</p>
<p>String diagram showing comonoid $\Delta$ for $H \otimes K$.</p>
<p>$\mu$ and $\eta$ should be a comonoid morphism, <em>i.e.</em> must commute with $\Delta$ (string diagram) and also with $\varepsilon$ (another string diagram).</p>
<p>There seems to be some asymmetry: monoid + comonoid + monoid must be comonoid morphisms. But it’s the same to say that the comonoid must be monoid morphisms.</p>
<h2 id="ends">Ends</h2>
<h3 id="ends-1">Ends 1</h3>
<p>http://youtu.be/mxI9ba6Rexc</p>
<p>Given a functor $T : C^{op}\times C \to D$, an <em>end</em> $\int_{c \in C} T(c,c)$ is an object in $D$ which is “limit-like” in some sense.</p>
<p>Ends are not as common as coends (and perhaps not as intuitive?). Two particular places where ends do show up:</p>
<ul>
<li>natural transformations (especially in enriched setting; see Ends 2)</li>
<li>reconstruction theorems (recover an algebra from category of its representations, <em>i.e.</em> <em>Tannaka reconstruction</em>, see Ends 3)</li>
</ul>
<p>Definition:</p>
<ul>
<li>A <em>wedge</em> $x \stackrel{\bullet}{\to} T$ consists of
<ul>
<li>an object $x \in D$</li>
<li>a family of $D$-morphisms $\omega_c : x \to T(c,c)$ for all $c \in C$</li>
<li>such that for all $f : c \to c'$ the obvious square with vertices $x$, $T(c,c)$, $T(c',c')$, and $T(c,c')$ commutes. (Dinaturality/extranaturality.)</li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/mrpq76b.png" />

</div>
</div>
<ul>
<li>This is in some sense a generalization of a cone.</li>
</ul></li>
<li><p>An <em>end</em> is a universal wedge, <em>i.e.</em> a wedge $E  \stackrel{\bullet}{\to} T$ such that if $x \stackrel{\bullet}{\to}  T$ then there exists a unique morphism $x \to E$ through which the components of $x$ factor.</p></li>
</ul>
<p>Note we write the object $E$ using the intergral notation, $\int_{c \in C} T(c,c)$ (the morphisms of the wedge are left implicit).</p>
<h3 id="ends-2">Ends 2</h3>
<p>http://youtu.be/gyc86NFT0Sw</p>
<p>Simple example of an end: $T = \mathrm{Hom} : C^{\text{op}} \times C  \to \mathbf{Set}$. In this case a wedge $x \stackrel{\bullet}{\to}  T$ consists of:</p>
<ul>
<li>some $x \in \mathbf{Set}$</li>
<li>for each $c \in C$ a function $\omega_c : x \to  \mathrm{Hom}(c,c)$</li>
<li>such that $\forall f : c \to c'$ we have $\omega_c; f \circ - =  \omega_{c'}; - \circ f$.</li>
</ul>
<p>That is, for every $n \in x$ we have $\omega_c(n) = n_c : c \to c$, such that $f \circ n_c = c_{c'} \circ f$. <em>i.e.</em> the family $n_c$ are the components of a natural transformation $Id_C \to Id_C$.</p>
<p>Note this goes in the other direction too, that is, a wedge $x \stackrel{\bullet}{\to} \mathrm{Hom}$ is precisely the same thing as a function $x \to \mathrm{Nat}(Id_C, Id_C)$. Therefore, the universal such $x$ is precisely this set of natural transformations. (Can be thought of as “set of symmetries” of a category. Also the <em>Hochschild cohomology</em>.)</p>
<h3 id="ends-3">Ends 3</h3>
<p>http://youtu.be/TfSUxhCNZZ0</p>
<p>More examples. First, straightforward generalization: given functors $F, G : C \to E$, form the bifunctor $\mathrm{Hom}_E(F(-), G(-)) : C^{op}\times C \to \mathbf{Set}$. Then we can see that</p>
<p>$\int_{c \in C} \mathrm{Hom}_E(F(c),G(c)) = \mathrm{Nat}(F,G)$.</p>
<p>(Proof is just a small generalization of the proof in Ends 2, left as exercise.) Useful in an enriched context, can use this end to construct an <em>object</em> of natural transformations instead of a set.</p>
<p>Another example, “baby Tannaka reconstruction” (see <a href="http://ncatlab.org/nlab/show/Tannaka+duality">Tannaka duality</a> and <a href="http://ncatlab.org/nlab/show/reconstruction+theorem">reconstruction theorem</a> on nlab).</p>
<ul>
<li>$M$ is a monoid in the category of sets.</li>
<li>$M$-$\mathbf{Set}$ is the category of sets that $M$ acts on</li>
<li>$U : M\text{-}\mathbf{Set} \to \mathbf{Set}$ is the forgetful functor.</li>
<li>Result: $\int_{(S,\sigma) \in M\text{-}\mathbf{Set}} \mathrm{Hom}  (U(S,\sigma), U(S,\sigma)) \cong \mathrm{Nat}(U,U) \cong M$. (In general, natural transformations over forgetful functor reconstructs algebraic objects.)</li>
</ul>
<p>Proof (application of Yoneda):</p>
<ul>
<li>Let $\mu : M \times M \to M$ be monoid operation and $e$ the identity.</li>
<li>An $M$-set consists of a set $S$ together with an action $\sigma : M  \times S \to S$. Morphisms $(S,\sigma) \to (R,\rho)$ are just functions $S \to R$ which commute with the actions (“equivariant maps”).</li>
<li>Note $U$ is representable:
<ul>
<li>Consider the $M$-set $M^l = (M, \mu)$ (“left-regular representation of $M$”)</li>
<li>Define $\mathrm{Hom}(M^l, (S, \sigma)) \to  S$ by $f \mapsto f(e)$.</li>
<li>Note $f(e)$ determines $f$ since $f$ is equivariant: $f(m) = f(m  \cdot e) = m \cdot f(e)$. In fact $\mathrm{Hom}(M^l, (S,  \sigma)) \cong S$.</li>
<li>Thus $U = \mathrm{Hom}(M^l, -)$.</li>
</ul></li>
<li>So $\mathrm{Nat}(U,U) = \mathrm{Nat}(\mathrm{Hom}(M^l,-),  \mathrm{Hom}(M^l,-))$, and by (co-?)Yoneda, this is just $\mathrm{Hom}(M^l, M^l) = U(M^l) = M$.</li>
</ul>
<h3 id="ends-4">Ends 4</h3>
<p>http://youtu.be/3hOtm0paWXY</p>
<p>Combine some of the previous examples. Recall</p>
<ul>
<li>$\int_C \mathrm{Hom} \cong \mathrm{Nat}(Id,Id)$ (Ends 2)</li>
<li>$\int_{M\text{-}\mathbf{Set}} \mathrm{Hom}_{\mathbf{Set}}(U(-),  U(-)) \cong M$ (Ends 3)</li>
</ul>
<p>What happens if we combine these two results? First, look at the end from last time:</p>
<ul>
<li>Let $\Phi \in \int_{M\text{-}\mathbf{Set}}  \mathrm{Hom}_{M\text{-}\mathbf{Set}}(U -,U -) = \mathrm{Nat}(U,U)$ be a natural transformation on $U$. That is, $\Phi_{(R,\rho)}$ is a function $R \to R$, such that $\Phi$ commutes with the underlying function of any equivariant map, <em>i.e.</em></li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/qzxv7gq.png" />

</div>
</div>
<ul>
<li>As we showed last time, $\Phi_{(R,\rho)} = \rho(m)$ for some $m \in  M$.</li>
<li>Note $\Phi_{(R,\rho)}$ is just a function $R \to R$, but has to commute with equivariant functions.</li>
</ul>
<p>Now look at the end of the bare hom-functor in the category of $M$-sets. <em>i.e.</em> $\int_{M\text{-}\mathbf{Set}} \mathrm{Hom}_{M\text{-}\mathbf{Set}}(-,-) = ?$</p>
<ul>
<li><p>Now if $\Theta \in \int_{M\text{-}\mathbf{Set}} \mathrm{Hom}_{M\text{-}\mathbf{Set}}(-,-)$, we have</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/mal2e6h.png" />

</div>
</div></li>
<li><p>What’s the difference? $\Theta$ is now a family of <em>equivariant</em> maps. But note equivariant maps are determined by their underlying function. So any diagram of this form implies one of the previous form; the only thing we’ve added is that $\Theta$ itself has to be equivariant (in the previous case $\Phi$ could be any function). So in fact we have</p>
<p>$\int \mathrm{Hom}(-,-) \subseteq \int \mathrm{Hom}(U -, U -) = M$.</p>
<p><em>i.e.</em> we’re picking out some subset of $M$. Question: <em>which</em> subset is it? That is, given such a $\Theta_{(R,\rho)}$ we know $\Theta_{(R,\rho)} = \rho(m)$ for some $m$; which $m$’s can we get?</p></li>
<li><p>Consider the left-regular representation $M^l$ again. Then we know $\Phi_{M^l} : M^l \to M_l$ is just left-multiplication by some $m  \in M$. But it has to commute with equivariant maps; picking the action on the particular element $e$, this means for all $n \in M$</p>
<p>$n \cdot \Phi_{M^l}(e) = \Phi_{M^l}(n \cdot e)$</p>
<p>that is, $nm = mn$, <em>i.e.</em> $m \in Z(M)$.</p></li>
<li><p>So we conclude $\int_{M\text{-}\mathbf{Set}}  \mathrm{Hom}_{M\text{-}\mathbf{Set}}(-,-) = Z(M)$.</p></li>
</ul>
<h2 id="adjunctions-from-morphisms">Adjunctions from morphisms</h2>
<h3 id="adjunctions-from-morphisms-1">Adjunctions from morphisms 1</h3>
<p>https://www.youtube.com/watch?v=SzzHjpRmrLU</p>
<p>General phenomenon: associate some category $C(X)$ to an object $X$. For example:</p>
<ul>
<li>In representation theory, to a group or algebra we associate a category of modules or representations.</li>
<li>In algebraic topology, to a space $X$ we associate $\mathbf{Vect}(X)$ (category of “bundles”?)</li>
<li>In algebraic geometry, to an algebraic variety associate the category of sheaves.</li>
<li>In logic, to a set of terms associate a category of subsets (predicates) over the terms.</li>
<li>In analysis, to a metric space $X$ associate a category of Lipschitz functions $X \to \mathbb{R}$.</li>
</ul>
<p>Question: if we have a morphism $f : X \to Y$, how does that relate to the categories $C(X)$ and $C(Y)$ associated to $X$ and $Y$?</p>
<p>We often get some sort of “pullback” functor $f^* : C(X) \leftarrow C(Y)$. (Also often get some sort of monoidal structure on $C(X)$ and $C(Y)$, and $f^*$ is often monoidal.)</p>
<p>We also get various “pushforwards” $f_* : C(X) \to C(Y)$, right adjoint to $f^*$. In some situation we also get a left adjoint to $f^*$.</p>
<p>This is the beginning of the story of “Grothendieck’s 6 operations”. Lots of similar structure arises in all these different areas.</p>
<h3 id="adjunctions-from-morphisms-2">Adjunctions from morphisms 2</h3>
<p>https://www.youtube.com/watch?v=jAQfNGEOass</p>
<p>Baby examples of some particular adjunctions (in generality, they show up in Grothendieck’s 6 operations, Frobenius reciprocity, …). Idea: start with (<em>e.g.</em>) sets; to each set associate a category; to each morphism between sets we will get functors between the categories.</p>
<ul>
<li>To the set $X$ associate the slice category $\mathbf{Set}/X$.
<ul>
<li>Think of the objects of this slice category, $E \to X$, as “bundles over $X$”: a base space $X$ and a set above that, where each element of $X$ is associated to its fiber/preimage.</li>
<li>Another way to think of this is as a functor $\hat{E} : X \to  \mathbf{Set}$ (considering $X$ as a discrete category), that picks out the fiber of each element of $X$.</li>
</ul></li>
<li><p>There is actually an equivalence of categories $\mathbf{Set}^X \cong  \mathbf{Set}/X$.</p></li>
<li><p>What about maps between sets? <em>e.g.</em> $f : X \to Y$. As we’ll see, we get three associated maps $f^* : \mathbf{Set}^Y \to  \mathbf{Set}^X$, $f_* : \mathbf{Set}^X \to \mathbf{Set}^Y$, and $f_! : \mathbf{Set}^X \to \mathbf{Set}^Y$, with $f_! \dashv f^*  \dashv f_*$. Details in the next lecture.</p></li>
</ul>
<h3 id="adjunctions-from-morphisms-3">Adjunctions from morphisms 3</h3>
<p>https://www.youtube.com/watch?v=tWhB7E-HS8Y</p>
<ul>
<li><p>Given $f : X \to Y$, define the “pullback” of a bundle over $Y$ to a bundle over $X$ written $f^* : \mathbf{Set}^Y \to \mathbf{Set}^X$: to each $x \in X$ we associate the fiber of $f(x) \in Y$. That is,</p>
<p>$f^* \hat{F} = \hat{F} \circ f.$</p></li>
<li><p>Now for the other direction. Idea: given a bundle over $X$ and $f :  X \to Y$, for each $y \in Y$ we have a set $f^{-1}(y) \subseteq X$ which are sent to that $y$ by $f$; we have to somehow combine their fibers to yield a fiber for $y$. Several ways we could imagine doing it: disjoint union, product? In this case we want the product. That is,</p>
<p>$\displaystyle (f_* \hat{E})(y) = \prod_{x \in f^{-1}(y)} \hat{E}(x).$</p></li>
</ul>
<p>(Foreshadowing: taking the disjoint union gives us another adjoint.)</p>
<h3 id="adjunctions-from-morphisms-4">Adjunctions from morphisms 4</h3>
<p>http://youtu.be/Cf7hCiTspJc</p>
<p>Proof of the adjunction $f^* \dashv f_*$. (Come up with your own mnemonic to remember which way around the adjunction goes; suggested: think of a “falling star”.)</p>
<ul>
<li><p>Notation: $\mathrm{Hom}_X(a,b)$ instead of $\mathbf{Set}^X(a,b)$ or $\mathrm{Hom}_{\mathbf{Set}^X}(a,b)$</p></li>
<li><p>Such hom-sets are a collection of maps between fibers, one for each base point.</p></li>
<li><p>So we have $\mathrm{Hom}_X(f^* \hat F, \hat E) = \prod_{x \in X}  \mathrm{Hom}(f^* \hat F (x), \hat E (x)) = \prod_{x \in X}  \mathrm{Hom}(\hat F (f(x)), \hat E (x))$.</p></li>
<li><p>We can partition $X$ as preimages of elements of $Y$ under $f$. So the above is equal to $\prod_{y \in Y} \prod_{x \in f^{-1}(y)}  \mathrm{Hom}(\hat F (y), \hat E (x))$.</p></li>
<li><p>A product of hom-sets is isomorphic to a hom-set into a product (<em>i.e.</em> $A^X B^X = (AB)^X$), so this is equal to $\prod_{y \in Y}  \mathrm{Hom}(\hat F (y), \prod_{x \in f^{-1}(y)} \hat E (x))$.</p></li>
<li><p>By definition of $f_*$, this is $\prod_{y \in Y}  \mathrm{Hom}(\hat F (y), (f_* \hat E) (y))$.</p></li>
<li><p>Finally, by definition of $\mathrm{Hom}_Y$, this is $\mathrm{Hom}_Y (\hat F, f_* \hat E)$.</p></li>
<li><p>Of course technically we would need to show naturality as well, but this is the basic idea.</p></li>
</ul>
<h3 id="adjunctions-from-morphisms-5">Adjunctions from morphisms 5</h3>
<p>https://www.youtube.com/watch?v=MSOGEtW39qM</p>
<p>Last time, we proved an adjunction $f^* \dashv f_*$, <em>i.e.</em></p>
<p>$\mathrm{Hom}_X (f^* \hat F, \hat E) \cong \mathrm{Hom}_Y (\hat F, f_* \hat E).$</p>
<p>In fact, we showed that both are isomorphic to</p>
<p>$\prod_{x \in X} \mathrm{Hom}(\hat F (f(x)), \hat E(x)).$</p>
<p><em>i.e.</em> given some $f : X \to Y$, for each $x$ we get a map going the <em>other</em> way, from the fiber over $f(x)$ to the fiber over $x$. (See the video for a nice picture.) But we can imagine turning these maps around, giving</p>
<p>$\prod_{x \in X} \mathrm{Hom}(\hat E(x), \hat F(f(x))).$</p>
<p>Using the same trick as last time, this is equivalent to $\prod_{y \in Y} \prod_{x \in f^{-1}(y)} \mathrm{Hom} (\hat E (x), \hat F (y))$, which is in turn equivalent to</p>
<p>$\prod_{y \in Y} \mathrm{Hom} (\coprod_{x \in f^{-1}(y)} \hat E (x), \hat F (y))$</p>
<p>(since $\mathrm{Hom}(-,B)$ turns limits into colimits; concretely, note that $A^X A^Y = A^{X + Y}$).</p>
<p>This gives us a left adjoint $f_! \dashv f^*$, defined by</p>
<p>$\displaystyle (f_! \hat E)(y) = \coprod_{x \in f^{-1}(y)} \hat E(x).$</p>
<p>Remark: note that if we view bundles over $X$ as objects of the slice category $\mathbf{Set}/X$, $f_!$ is just composition.</p>
<h2 id="double-categories">Double Categories</h2>
<h3 id="double-categories-1">Double Categories</h3>
<p>https://www.youtube.com/watch?v=kiCZiSA2W3Q</p>
<p>Internal categories in $\mathbf{Cat}$. Recall that an internal category in $E$ is a pair of objects $A$ (representing objects) and $B$ (representing morphisms), and a pair of parallel arrows $s,t : B \to A$ in $E$ recording the source and target of each morphism, all suitably equipped with unit and composition.</p>
<p>If $A$ and $B$ are themselves categories, and $s$ and $t$ are functors, then $B$ itself has sets of objects $B_0$ and morphisms $B_1$ with source and target functions, and the same for $A$. Then the functors $s$ and $t$ have actions on morphisms and objects, so we get a square with two parallel arrows on each side.</p>
<ul>
<li>$A_0$ are $0$-cells.</li>
<li>$A_1$ are “vertical $1$-cells”.</li>
<li>$B_0$ are “horizontal $1$-cells”.</li>
<li>$B_1$ are $2$-cells, which sit inside <em>squares</em> (not inside the area between two parallel $1$-cells): each element of $B_1$ has corresponding sources and targets in both $B_0$ and $A_1$, and the double commuting square described above ensures that the sources and targets of <em>those</em> have to match up in a square.</li>
</ul>
<p>What about composition? Note $B$ and $A$ already come equipped with composition, which together give us “vertical composition” of $2$-cells. Composition in the internal category gives horizontal composition of $2$-cells.</p>
<p>Note if all vertical $1$-cells are identities, this collapses to the usual idea of a $2$-category. (Or symmetrically, with horizontal $1$-cells being identities.)</p>
<h2 id="spans">Spans</h2>
<h3 id="spans-1">Spans 1</h3>
<p>https://www.youtube.com/watch?v=SQfUXOCMUhI</p>
<p>NOTE: There seems to be no catsters video actually explaining what a “bicategory” is. <a href="https://ncatlab.org/nlab/show/bicategory">According to the nlab</a> it is a weaker version of a 2-category, where certain things are required to hold only up to coherent isomorphism rather than on the nose.</p>
<p>Let $E$ be a category with (chosen) pullbacks. $\mathbf{Span}(E)$ is a bicategory with</p>
<ul>
<li>$0$-cells the objects of $E$</li>
<li>$1$-cells $A \to B$ are spans $A \leftarrow S \rightarrow B$.</li>
<li><p>$2$-cells are morphisms between $1$-cells, that is, spans. So a $2$-cell between $A \leftarrow S \rightarrow B$ and $A \leftarrow S'  \rightarrow B$ is a morphism $S \to S'$ which makes things commute.</p></li>
<li><p>$1$-cell composition: use pullback. Is this associative? Yes, up to isomorphism (because of universal property of pullbacks) but not on the nose. (Hence we get a bicategory and not a strict $2$-category.)</p></li>
<li>Vertical $2$-cell composition: just composition of the underlying morphisms.</li>
<li><p>Horizontal $2$-cell composition: the two pullbacks induce a morphism between them.</p></li>
</ul>
<p>Can check all the axioms etc.</p>
<p>Now, note monads can be constructed inside any bicategory, and are given by</p>
<ul>
<li>a $0$-cell $X$</li>
<li>a $1$-cell $T : X \to X$</li>
<li>$2$-cells $\eta$ and $\mu$ satisfying the usual monad axioms (slightly modified to make them make sense)</li>
</ul>
<p>It turns out that monads in $\mathbf{Span}(E)$ are great! For example, monads in $\mathbf{Span}(\mathbf{Set})$ are small categories. Next time we’ll see why.</p>
<h3 id="spans-2">Spans 2</h3>
<p>https://www.youtube.com/watch?v=Jn5dZuebeXU</p>
<p>Monads in $\mathbf{Span}(Set)$ are small categories. These notes make a lot more sense when you can look at the diagrams. Watch the video or work out the diagrams yourself.</p>
<p>We have</p>
<ul>
<li>a $0$-cell, <em>i.e.</em> a set $C_0$.</li>
<li>a $1$-cell from $C_0$ to itself, <em>i.e.</em> a span $C_0  \stackrel{s}{\leftarrow} C_1 \stackrel{t}{\rightarrow} C_0$ (idea is that $C_1$ will be the set of morphisms, and $s$ and $t$ will pick out the source and target objects)</li>
<li>a $2$-cell $\eta$ from $1$ (the boring span with all the same object and identity morphisms) to the $1$-cell given above. This ends up being a function $f : C_0 \to C_1$ such that $f ; s = f ; t = id$, that is, $f$ takes each object in $C_0$ to a morphism in $C_1$ having that object as both its source and target.</li>
<li>a $2$-cell $\mu : C_1^2 \to C_1$. $C_1^2$ is given by a pullback: a pair of morphisms such that the target of the first equals the source of the second, <em>i.e.</em> a composable pair. $\mu$ therefore has to take a composable pair and produce a single morphism in $C_1$ such that its source equals the source of the first and its target equals the target of the second.</li>
</ul>
<p>And of course there are some monad laws which amount to the category laws.</p>
<p>More generally, monads in $\mathbf{Span}(E)$ are categories internal to $E$. <em>I.e.</em></p>
<ul>
<li>an “objects object” $C_0$</li>
<li>a “morphisms object” $C_1$</li>
<li>source and target maps $C_1 \to C_0$</li>
<li>identities and composition as before.</li>
</ul>
<h2 id="multicategories">Multicategories</h2>
<h3 id="multicategories-1">Multicategories 1</h3>
<p>http://www.youtube.com/watch?v=D_pPNgGZYDs</p>
<p>Like categories, but morphisms have multiple objects as their source.</p>
<p>A (small) multicategory $C$ is given by</p>
<ul>
<li>a set of objects, $\mathrm{ob} C$</li>
<li>For all $x_1, \dots, x_n, x$, a set $C(x_1, \dots, x_n, x)$ of morphisms.</li>
<li>Composition: a morphism with $n$ inputs can be composed with $n$ morphisms, producing a result which is a morphism with the concatenation of inputs of all the $n$ morphisms.</li>
<li>Identity morphisms, with just one input and output.</li>
</ul>
<p>Note that one can have a morphism with <em>no</em> inputs.</p>
<p>This can all be expressed nicely using the “free monoid monad” (i.e. list monad). Let $T$ be the free monoid monad on $\mathbf{Set}$, i.e. the list monad; that is, $T$ sends each set $S$ to the free monoid on $S$ (i.e. lists of $S$).</p>
<p>Make a bicategory of $T$-spans. Just as monads in the category of spans were small categories, monads in the (bi)category of $T$-spans are multicategories.</p>
<p>$T$-span has:</p>
<ul>
<li>$0$-cells are sets</li>
<li>$1$-cells are spans $T A \leftarrow S \to B$.</li>
<li>$2$-cells are just span morphisms.</li>
<li>Composition uses pullbacks and multiplication of $T$.</li>
<li>Identity is $T$-span $T A \leftarrow A \to A$ using $\eta$ and $id$.</li>
</ul>
<p>Bicategory axioms follow from monad laws for $T$. Next time: monads in this category are multicategories.</p>
<h3 id="multicategories-2">Multicategories 2</h3>
<p>https://www.youtube.com/watch?v=WytjdlserwU</p>
<p>We’ve seen that monads in Span are categories.</p>
<p>We’ve seen a category of $T$-spans, spans with a $T$ on the left. We’ll see that monads in $T$-$\mathbf{Span}$ are multicategories.</p>
<p>Recall that $T$ is the list monad.</p>
<p>A monad in $T$-$\mathbf{Span}$ is:</p>
<ul>
<li>a set $C_0$ (which will represent objects of the multicategory)</li>
<li>a $1$-cell $C_0 \to C_0$ is a $T$-span, i.e. an object $C_1$ (representing morphisms of the multicategory) together with morphisms from $C_1$ to $T\ C_0$ (picking out the sequence of input objects) and from $C_1$ to $C_0$ (picking the target object).</li>
<li>a $2$-cell $\eta$, representing the identity morphism with a single input (see video for a commutative diagrma)</li>
<li>a $2$-cell $\mu$ which represents composition in a multicategory. See video for diagram!</li>
</ul>
<p>Key point: we can actually do this with other monads $T$! And even on other categories with pullbacks, as long as $T$ preserves pullbacks (and $\eta$ and $\mu$ commutative diagrams are pullbacks). This yields a notion of a $T$-multicategory. The source of each morphism is not just a list of objects but a $T$-structure of objects.</p>
<h2 id="metric-spaces-and-enriched-categories">Metric Spaces and Enriched Categories</h2>
<h3 id="metric-spaces-and-enriched-categories-1">Metric Spaces and Enriched Categories 1</h3>
<p>https://www.youtube.com/watch?v=be7rx29eMr4</p>
<p>Idea due to Lawvere. A metric $d$ on a metric space satisfies:</p>
<ul>
<li>Triangle inequality: $d(a,b) + d(b,c) \geq d(a,c)$</li>
<li>$0 \geq d(a,a)$</li>
</ul>
<p>Compare to the data for a category, written in a slightly funny way:</p>
<ul>
<li>$\mathrm{Hom}(a,b) \times \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$</li>
<li>$\{\star\} \to \mathrm{Hom}(a,a)$</li>
</ul>
<p>These look remarkably similar! In fact, they are both examples of enriched category. We’ll start with a normal category and show how to generalize it to an enriched category.</p>
<p>Let $C$ be a category. We have:</p>
<ul>
<li>a collection $\mathrm{Ob}(C)$</li>
<li>$\forall a,b \in \mathrm{Ob}(C)$, $\mathrm{Hom}(a,b) \in \mathrm{Ob}(\mathbf{Set})$</li>
<li>$\forall a,b,c \in \mathrm{Ob}(C)$, $\exists \mathrm{Hom}(a,b) \times \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$</li>
<li>$\forall a \in \mathrm{Ob}(C)$, $\exists \{\star\} \to \mathrm{Hom}(a,a)$</li>
<li>…satisfying associativity and unit laws.</li>
</ul>
<p>Important thing to note: composition and identity are morphisms in $\mathbf{Set}$. What properties of $\mathbf{Set}$ have we used? Just a Cartesian product and the one-element set $\{\star\}$. Right generalization is a monoidal category.</p>
<p>In particular, if $(V, \otimes, I)$ is a monoidal category, we can define <em>categories enriched in $V$</em>. Definition becomes:</p>
<p>$C$ is a $V$-category (category enriched in $V$):</p>
<ul>
<li>collection $\mathrm{Ob}(C)$ as before</li>
<li>$\mathrm{Hom}(a,b) \in \mathrm{Ob}(V)$, <em>i.e</em> we don’t have hom-<em>sets</em> but hom-<em>objects</em> that live in the category $V$</li>
<li>The composition map is a morphism $\mathrm{Hom}(a,b) \otimes \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$ in $V$</li>
<li>Identity morphisms are now given by a morphism $I \to \mathrm{Hom}(a,a)$ in $V$.</li>
<li>…satisfying associativity and unit laws.</li>
</ul>
<p><em>e.g.</em> pick $V$ to be category of Abelian groups (yields “additive category”), or category of vector spaces (yields “linear categories”).</p>
<p>What if we take $V$ to be a non-concrete category? <em>e.g.</em> take the poset of nonnegative real numbers under $\geq$. Can make this monoidal by taking the usual $+$, identity is $0$. Then it turns out that categories enriched in this poset category are metric spaces!</p>
<h3 id="metric-spaces-and-enriched-categories-2">Metric Spaces and Enriched Categories 2</h3>
<p>https://www.youtube.com/watch?v=0p3iS3Nf-fs</p>
<p>Explains in more detail how categories enriched in $(\mathbb{R}^+, \geq)$ poset (with monoidal structure given by $+$ and $0$) are metric spaces.</p>
<ul>
<li>For each pair of objects we have a “Hom-object” which is a nonnegative real number. (“distance”)</li>
<li>Composition is supposed to be a morphism $\mathrm{Hom}(a,b) \otimes  \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$. But $\otimes$ is $+$ and $\to$ is $\geq$, so this is the triangle inequality.</li>
<li>Identity morphisms are given by $I \to \mathrm{Hom}(a,a)$; in this context that says $0 \geq \mathrm{Hom}(a,a)$, i.e. distance from $a$ to itself is $0$.</li>
<li>Associativity and unit laws are vacuous.</li>
</ul>
<p>This is actually a <em>generalized</em> metric space. More general than a metric space in several ways:</p>
<ul>
<li>Distance is not symmetric: $d(a,b) \neq d(b,a)$. This actually has many real-world models (e.g. time to go between towns in the Alps).</li>
<li>We might have $d(a,b) = 0$ for $a \neq b$.</li>
<li>We allow “infinite” distances (???)</li>
</ul>
<p>Now we can study metric spaces categorically.</p>
<p>Given two $V$-categories, a $V$-functor $\Phi : C \to D$ consists of</p>
<ul>
<li>a map $\mathrm{Ob}(C) \to \mathrm{Ob}(D)$, and</li>
<li>for all $a,b,c \in C$, $\Phi : \mathrm{Hom}(a,b) \to  \mathrm{Hom(\Phi a, \Phi b)}$ (a morphism in $D$) which has to commute with composition in the appropriate way.</li>
</ul>
<p>In the generalized metric setting, such a $\Phi$ is a nonexpansive map.</p>
<h3 id="metric-spaces-and-enriched-categories-3">Metric Spaces and Enriched Categories 3</h3>
<p>https://www.youtube.com/watch?v=kMSt_Ci54BE</p>
<p>Enriched natural transformations. There are actually various ways to generalize. Today: simple-minded version. Apparently if we generalize the notion of a <em>set</em> of natural transformations we get a slightly better definition—this will be covered in future videos. [editor’s note: to my knowledge no such future videos exist.]</p>
<p>Standard definition of a natural transformation $\theta : F \to G$ given functors $F,G : C \to D$. Problem: we are supposed to have $\theta_x \in \mathrm{Hom}(F x, G x)$, but in the enriched setting $\mathrm{Hom}(F x, G x)$ may not be a <em>set</em>, but just some object. So what should it mean for $\theta_x$ to be an “element” of it?</p>
<p>Simple way around this: let $(V, \otimes, 1)$ be a monoidal category. We can define a canonical functor $\Gamma : V \to \mathrm{Set}$ (“generalized elements”) which takes an object $v \in V$ to the Hom-set $\mathrm{Hom}(1,v)$.</p>
<p>e.g. if $V = \mathrm{Set}$, $\Gamma$ is the identity. Another example I don’t understand involving category of complex vector spaces.</p>
<p>In the example we care about, $V = \mathbb{R}^+$, and $I = 0$. In this case $\Gamma$ sends $v$ to the Hom-set $0 \geq v$, that is, $\{\star\}$ if $v = 0$ and the empty set otherwise.</p>
<p>So now we can say that $\theta_x$ should be a <em>generalized element</em> of $\mathrm{Hom}(F x, G x)$.</p>
<p>So, let $F, G$ be $V$-functors. Define a $V$-natural transformation $\theta : F \to G$ as</p>
<ul>
<li>for all $x \in C$, $\theta_x \in \Gamma(\mathrm{Hom}(F x, G x))$.</li>
<li>Big commuting diagram expressing naturality in the enriched setting (see the video).</li>
</ul>

