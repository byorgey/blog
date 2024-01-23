---
title: Catsters guide
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
<li>$latex B \times A$ and $latex A \times B$ as two (isomorphic) products</li>
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
<li>Definition of the morphisms $latex (f,g)$ and $latex f \times g$</li>
<li>The diagonal</li>
<li>Products with the terminal object</li>
</ul>
<h2 id="pullbacks-and-pushouts">Pullbacks and Pushouts</h2>
<h3 id="pullbacks-and-pushouts-1">Pullbacks and pushouts 1</h3>
<p>http://youtu.be/XGysPJvCXOc</p>
<ul>
<li>Definition of pullback</li>
<li>Example: pullbacks in $latex \mathbf{Set}$</li>
</ul>
<h3 id="pullbacks-and-pushouts-2">Pullbacks and pushouts 2</h3>
<p>http://youtu.be/LkkallToFQ0</p>
<ul>
<li>Definition of pushouts</li>
<li>Example: pushouts in $latex \mathbf{Set}$</li>
<li>Pullback/pushout example of intersection/union of sets</li>
</ul>
<h2 id="natural-transformations">Natural transformations</h2>
<h3 id="natural-transformations-1">Natural transformations 1</h3>
<p>http://www.youtube.com/watch?v=FZSUwqWjHCU</p>
<ul>
<li>Definition of natural transformations.</li>
<li>Naturality squares.</li>
<li>Intuition about natural transformations based on homotopy.</li>
<li>Alternative definition of natural transformation analogous to usual homotopy definition: a natural transformation is a functor $latex C \times  I \to D$ where $latex I$ is the “categorical interval”, <em>i.e.</em> the two-object category with a single nontrivial morphism.</li>
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
<li>Definition of representable functors (co- or contravariant): those which are naturally isomorphic to $latex H^A$ or $latex H_A$ for some $latex A$.
<ul>
<li>Contravariant: $latex H_A : C^{\text{op}} \to \mathbf{Set}$; $latex H_A(X) = C(X,A)$.</li>
<li>Covariant: $latex H^A : C \to \mathbf{Set}$; $latex H^A(X) = C(A,X)$.</li>
</ul></li>
<li>Is there a functor $latex A \to H_A$? Yes, the <em>Yoneda embedding</em> $latex Y$:
<ul>
<li>$latex Y(A) = H_A$</li>
<li>$latex Y(f) = H_f$, where $latex H_f : H_A \to H_B$ is postcomposition with $latex f$. $latex H_f$ is a natural transformation; its component at $latex X$ has type $latex H_f(X) : H_A(X) \to H_B(X) = C(X,A) \to  C(X,B)$. Postcomposing an arrow in $latex C(X,A)$ with $latex f : A  \to B$ yields an arrow in $latex C(X,B)$.</li>
</ul></li>
</ul>
<h3 id="representables-and-yoneda-2">Representables and Yoneda 2</h3>
<p>http://youtu.be/eaJmUUogb6g</p>
<ul>
<li>Proof that Yoneda embedding $latex C \to [C^{\text{op}}, \mathbf{Set}]$ sends morphisms $latex f$ to <em>natural transformations</em> $latex H_f$. Comes down to the fact that composition in the category is associative.</li>
</ul>
<h3 id="representables-and-yoneda-3">Representables and Yoneda 3</h3>
<p>http://youtu.be/TLMxHB19khE</p>
<ul>
<li>Look at natural transformations from $latex H_A$ to some other (contravariant) functor $latex F$. Big idea: such natural transformations $latex \alpha$ are entirely determined by where $latex \alpha_A$ sends $latex 1_A$.</li>
<li>Yoneda lemma: $latex F\ A \cong [C^{\text{op}}, \mathbf{Set}](H_A, F)$ (natural in $latex A$ and $latex F$). <em>I.e.</em> the set of objects in $latex F\ A$ is isomorphic to the hom-set of natural transformations between $latex H_A$ and $latex F$.</li>
</ul>
<h2 id="adjunctions-part-1">Adjunctions (part 1)</h2>
<h3 id="adjunctions-1">Adjunctions 1</h3>
<p>http://youtu.be/loOJxIOmShE</p>
<ul>
<li>Given categories $latex C$ and $latex D$ and functors $latex F : C \to D$ and $latex G : D  \to C$, we have the following situations:
<ul>
<li>Isomorphism: $latex 1 = GF$, $latex FG = 1$</li>
<li>Equivalence: $latex 1 \cong GF$, $latex FG \cong 1$</li>
<li>Adjunction: $latex \eta : 1 \Rightarrow GF$, $latex \varepsilon : FG \Rightarrow 1$ So we can think of an adjunction as a “weaker sort of equivalence”.</li>
</ul></li>
<li>$latex \eta$ and $latex \varepsilon$ are subject to triangle identities: $latex F  \stackrel{F\eta}{\Longrightarrow} FGF \stackrel{\varepsilon  F}{\Longrightarrow} F$ is the identity, and similarly for $latex \eta G ; G \varepsilon$.</li>
<li>These laws can be expressed as commuting diagrams of 2-cells: draw $latex \eta$ and $latex \varepsilon$ as 2-cells and paste them in two different ways.</li>
</ul>
<h3 id="adjunctions-2">Adjunctions 2</h3>
<p>http://youtu.be/JEJim3t-N9A</p>
<ul>
<li>Alternate definition of adjunction $latex F \dashv G$: an isomorphism $latex D(F X, Y) \cong C(X, G Y)$ natural in $latex X$ and $latex Y$.</li>
<li>What “natural in $latex X$ and $latex Y$” means here.</li>
<li>Hint: sending identity morphisms across the iso gives us $latex \eta$ and $latex \varepsilon$ from the first definition. Proof deferred to Adjunctions 4.</li>
</ul>
<h3 id="adjunctions-4">Adjunctions 4</h3>
<p>http://youtu.be/nP5XQ6OBHHY</p>
<ul>
<li>Note: Adjunctions 4, not 3, follows on to 2.</li>
<li>Given: an isomorphism $latex D(FX,Y) \cong C(X,GY)$ which is natural in $latex X$ and $latex Y$.</li>
<li>Notation: write application of the isomorphism as an overbar.</li>
<li>Construct the two squares implied by naturality. Follow them each around in <em>both</em> directions (since they involve a natural <em>isomorphism</em>) to get four equations in total governing how the iso interacts.</li>
<li>Define $latex \eta$ and $latex \varepsilon$ by applying the isomorphism to appropriate identity morphisms. Naturality and the triangle identities follow from the above four equations.</li>
</ul>
<h2 id="monads">Monads</h2>
<h3 id="monads-1">Monads 1</h3>
<p>http://youtu.be/9fohXBj2UEI</p>
<ul>
<li>Monads give us a way to talk about algebraic theories (monoids, categories, groups, <em>etc.</em>).</li>
<li>Definition of a monad:
<ul>
<li>Functor $latex T : C \to C$</li>
<li>“unit” $latex \eta : 1 \Rightarrow T$</li>
<li>“multiplication” $latex \mu : T^2 \Rightarrow T$</li>
<li>with unit and associativity laws.</li>
</ul></li>
<li>Note what is meant by a commutative diagram of natural transformations</li>
<li>Example: monad for monoids (aka the list monad)
<ul>
<li>$latex C = \mathbf{Set}$</li>
<li>$latex T = X \mapsto X^*$, maps a set $latex X$ to set of words in $latex X$ (<em>i.e.</em> lists)</li>
<li>$latex \eta$ is singleton</li>
<li>$latex \mu$ is concatenation</li>
<li>Note, unit and associativity for <em>monad</em> is different than unit and associativity of <em>monoids</em>, which has already been encoded in the definition of $latex T$.</li>
</ul></li>
</ul>
<h3 id="monads-2">Monads 2</h3>
<p>http://youtu.be/Si6_oG7ZdK4</p>
<ul>
<li>Proof that the list monad (“monad for monoids”) is in fact a monad</li>
<li>Example: monad for small categories
<ul>
<li>$latex C = \mathbf{Gph}$, category of graphs</li>
<li>$latex T$ makes the free category on a graph (morphisms = paths in the underlying graph)</li>
<li>With only one object, this reduces to the monad for monoids.</li>
<li>Proof of monads laws is basically the same as for the list monad.</li>
</ul></li>
</ul>
<h3 id="monads-3">Monads 3</h3>
<p>http://youtu.be/eBQnysX7oLI</p>
<ul>
<li>Algebras for monads. Monads are supposed to be like algebraic theories; algebras are models.</li>
<li>An <em>algebra</em> for a monad $latex (T : C \to C, \eta, \mu)$ is an object $latex A  \in C$ (the “underlying object”) equipped with an “action” $latex \theta :  T\ A \to A$, satisfying the “obvious” axioms ($latex \theta$ must interact “sensibly” with $latex \eta$ and $latex \mu$).
<ul>
<li>$latex \eta ; \theta = 1$</li>
<li>$latex \mu ; \theta = T \theta ; \theta$</li>
</ul></li>
<li>Example: $latex C = \mathbf{Set}$, $latex T$ = list monad (“monad for monoids”)
<ul>
<li>An algebra is a set $latex A$ equipped with $latex \theta : [A] \to A$</li>
<li>First axiom says $latex \theta$ must simply project element out of length-one list.</li>
<li>Other axiom is essentially associativity.</li>
<li>That is, algebras for the list monad are monoids.</li>
</ul></li>
<li>Example for monad of categories (from last time) works the same way.</li>
</ul>
<h3 id="monads-3a">Monads 3A</h3>
<p>http://youtu.be/uYY5c1kkoIo</p>
<p>More on monoids as monad algebras of the list monad.</p>
<ul>
<li>Given a monad algebra $latex \theta : [A] \to A$, construct the monoid:
<ul>
<li>whose underlying set is $latex A$</li>
<li>$latex a \cdot b = \theta [a,b]$</li>
<li>$latex \varepsilon = \theta []$.</li>
</ul></li>
<li>The monad algebra law for $latex \eta$ (a triangle) just says that $latex \theta$ can’t do anything interesting on one-element lists: it has to just return the single element.</li>
<li>Identity and associativity laws for the monoid come from the other monad algebra law, saying how $latex \theta$ interacts with $latex \mu$ (a square), and from how the list functor is defined. We <em>start with</em> a way of mapping <em>lists</em> down to values, which bakes in the idea that it doesn’t matter how we associate the list.</li>
</ul>
<h3 id="monads-4">Monads 4</h3>
<p>http://youtu.be/Cm-O_ZWEIGY</p>
<p>Monad algebras form a category (called $latex \mathbf{Alg}\ T$).</p>
<ul>
<li><p>Given two monad algebras $latex \theta : T A \to A$ and $latex \varphi : T B \to  B$, a morphism between them consists of a morphism of underlying objects, $latex f : A \to B$, such that the obvious square commutes.</p></li>
<li><p>Example. List monad again. $latex C = \mathbf{Set}, T = []$. A morphism of monoids is a function $latex f : A \to B$ such that $latex f (xy) =  f(x)f(y)$. See how this equation arises from the commuting square for monad morphisms, by starting with a 2-element list in upper left and following it around.</p></li>
<li><p>Given a particular mathematical theory, can it be expressed as the category of algebras for some monad? <em>I.e.</em> given a category $latex D$, is it equivalent to $latex \mathbf{Alg}\ T$ for some $latex T$? (Answer: no, not in general, <em>e.g.</em> category of topological spaces can’t.)</p></li>
<li><p>But this is still an interesting question, more or less the question of “monadicity”. Category $latex D$ said to be <em>monadic</em> over category $latex C$ if $latex D$ can be expressed as category of algebras of monads over $latex C$.</p></li>
</ul>
<h2 id="adjunctions-and-monads">Adjunctions and monads</h2>
<h3 id="adjunctions-3">Adjunctions 3</h3>
<p>http://youtu.be/2i_PpYsl8b8</p>
<ul>
<li>Note: depends on monads.</li>
<li>Examples of adjunctions:
<ul>
<li>between the category of sets and the category of monoids: $latex \text{Free} \dashv \text{Forget}$</li>
<li>similarly between category of graphs and category $latex \mathbf{Cat}$ of (small) categories.</li>
</ul>
<p>In general, free functors are left adjoint to forgetful functors. (How to remember the direction: “left” has four letters, just like “free”.)</p></li>
<li>Every adjunction $latex (F \dashv G : C \to D, \eta, \varepsilon)$ gives rise to a monad $latex (GF, \eta, \mu = G \varepsilon F)$. Check monad laws:
<ul>
<li>Monad triangle laws are just adjunction triangle laws with extra $latex G$ or $latex F$ everywhere.</li>
<li>Monad associativity law is naturalty for $latex \varepsilon$, or something like that.</li>
</ul></li>
</ul>
<h3 id="adjunctions-5">Adjunctions 5</h3>
<p>http://youtu.be/xqLgGB7Hv7g</p>
<p>“Every monad comes from an adjunction via its category of algebras.”</p>
<p>Last time we showed every adjunction gives rise to a monad. What about the converse?</p>
<p>Answer: yes. In fact, given a monad, there is an entire category of adjunctions which give rise to it, which always has initial and terminal objects: these are the constructions found by Kleisli and by Eilenberg-Moore, respectively. Intuitively, any other adjunction giving rise to the monad can be described by the morphisms between it and the Kleisli and Eilenberg-Moore constructions.</p>
<p>Let $latex (T : C \to C, \eta, \mu)$ be a monad.</p>
<ul>
<li><p>Terminal solution (Eilenberg-Moore): consider category $latex \mathbf{Alg}\ T$ of $latex T$-algebras, also written $latex C^T$. We construct an adjunction $latex F \dashv G : C \to C^T$. (Intuition: $latex F : C \to C^T$ “freely” constructs a $latex T$-algebra; $latex G : C^T \to C$ “forgets” the algebra structure.)</p>
<ul>
<li><p>$latex G : C^T \to C$ is easy to construct: $latex (A, \theta : T A \to A)  \mapsto A$.</p></li>
<li><p>What about $latex F : C \to C^T$? Sends $latex A$ to the “free” $latex T$-algebra on $latex A$, with underlying set $latex T A$. Then evaluation map is $latex \mu$. That is, $latex F A = (T A, \mu : T (T A) \to T A)$. Need to check that this definition of $latex F$ really gives a monad algebra as a result. In this case the monad algebra laws are just the monad laws for $latex T$!</p></li>
<li><p>Now define a unit and counit. $latex \eta_A : A \to GFA = A \to TA$ is just the $latex \eta$ for the monad. $latex \varepsilon_\theta :  FG(A,\theta) \to (A,\theta)$ is an algebra morphism from the free algebra on $latex A$ (<em>i.e.</em> $latex (TA, \mu)$) to $latex (A,\theta)$: in fact, $latex \theta$ itself is such a morphism, by the second algebra law.</p></li>
<li><p>Prove triangle laws for $latex \eta$ and $latex \varepsilon$: exercise for the watcher/reader.</p></li>
</ul></li>
</ul>
<h3 id="adjunctions-6">Adjunctions 6</h3>
<p>http://youtu.be/Ht1mQ97Zq2k</p>
<p>This time, initial solution to “does a monad give rise to any adjunctions”: Kleisli.</p>
<ul>
<li>The Kleisli category for a monad $latex T$ on category $latex C$, written $latex \mathbf{Kl}(T)$ or $latex C_T$
<ul>
<li>Objects: objects of $latex C$.</li>
<li>Morphisms: $latex C_T(A,B) :\equiv C(A,T B)$.</li>
<li>Composition: given $latex f : A \to T B$ and $latex g : B \to T C$, produce $latex f ; T g ; \mu : A \to T C$.</li>
<li>Identity: $latex \eta : A \to T A$.</li>
<li>Category axioms come from monad axioms. Associativity comes from associativity and naturality of $latex \mu$; unit laws come from unit laws for $latex \eta$.</li>
</ul></li>
<li><p>Intuition: this is the category of free algebras: $latex A \to T B = GFB$ is equivalent, under the adjunction, to $latex F A \to F B$, morphism between free algebras.</p></li>
<li><p>Note, for the Eilenberg-Moore category (last time) it was complicated to define the objects and simple to define the morphisms. For Kleisli, it’s the other way around. “Conservation of complicatedness.”</p></li>
</ul>
<h3 id="adjunctions-7">Adjunctions 7</h3>
<p>http://youtu.be/D8g9xnVr0Lg</p>
<p>The adjunction that comes from the Kleisli category, giving rise to the original monad $latex T$.</p>
<p>Again, let $latex (T : C \to C, \eta, \mu)$ be a monad. We will construct $latex F_T \dashv G_T : C \to C_T$, where $latex C_T$ is the Kleisli category defined in Adjunctions 6, with $latex G_T F_T = T$.</p>
<ul>
<li>$latex F_T : C \to C_T$ sends objects to “free algebras”
<ul>
<li>Identity on objects.</li>
<li>On morphisms, sends $latex f : A \to B$ to $latex \eta ; T f : A \to T B$ (equivalently $latex f ; \eta$).</li>
</ul></li>
<li>$latex G_T : C_T \to C$ sends a “free algebra” to its “underlying object”
<ul>
<li>Sends $latex A$ to $latex T A$.</li>
<li>Sends $latex f : A \to T B$ to $latex T f ; \mu : T A \to T B$.</li>
</ul></li>
<li>Unit and counit
<ul>
<li>$latex \eta : A \to G_T F_T A = T A$ we can take as the $latex \eta$ of the monad.</li>
<li>$latex \varepsilon : F_T G_T A = T A \to T A$ we can take to be id.</li>
</ul></li>
<li>Adjunction laws come down to monad laws (left to viewer).</li>
</ul>
<p>Given a monad $latex T$ on $latex C$, we have a category of adjunctions $latex \mathbf{Adj}(T)$ giving rise to $latex T$ (morphisms are functors making everything commute). $latex C_T$ is the initial object and $latex C^T$ is terminal.</p>
<p>Question of <em>monadicity</em>: given an adjunction $latex F \dashv G$, is $latex D \cong C^T$? If so, say “$latex D$ is monadic over $latex C$”, <em>i.e.</em> everything in $latex D$ can be expressed as monad algebras of $latex C$. Or can say the adjunction is a “monadic adjunction”. Can also say that the right adjoint (forgetful functor $latex G$) “is monadic”. Monadic adjunctions are particularly nice/canonical.</p>
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
<p>Given an adjunction $latex F \dashv G$, we have natural transformations $latex \varepsilon : FG \to 1$ and $latex \eta : 1 \to GF$, and two laws given by triangles. What do these look like as string diagrams? $latex \varepsilon$ is a cap, $latex \eta$ a cup, and the triangle laws look like pulling wiggly strings straight!</p>
<h3 id="string-diagrams-4">String diagrams 4</h3>
<p>http://youtu.be/YNC5faXshAk</p>
<p>Monads in string diagrams. Draw $latex \mu$, $latex \eta$, and the monad laws as nice-looking string diagrams with nice topological intuition.</p>
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
<li>Pullback: limit over a “cospan”, <em>i.e.</em> a diagram like $latex X \to Y  \leftarrow Z$. Note that we usually ignore the edge of the cone to $latex Y$, since it is uniquely determined by the edges to $latex X$ and $latex Z$.</li>
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
<li><p>Formal definition of a limit: given a diagram $latex D : I \to C$, a limit for $latex D$ is an object $latex U$ together with a family of isomorphisms $latex C(V,U) \cong [I,C](\Delta_V, D)$ natural in $latex V$. <em>I.e.</em> a natural correspondence between morphisms $latex V \to U$ (the “factorization” from one cone to another) and morphisms (<em>i.e.</em> natural transformations) from $latex \Delta_V$ to $latex D$ in the functor category $latex [I,C]$ (<em>i.e.</em> cones over $latex D$ with vertex $latex V$). That is, every cone with vertex $latex V$ has a unique factorization morphism, <em>and vice versa</em>!. The “vice versa” part is the surprising bit. If we have a limit then <em>every</em> morphism is the factorization for some cone to the universal cone.</p></li>
<li><p>If we set $latex V = U$ then $latex C(U,U) \cong \dots$ <em>etc.</em> In particular $latex 1_U :  C(U,U)$ corresponds to some cone, which is THE universal cone. The Yoneda lemma says (?) that the entire natural isomorphism is determined by this one piece of data (where $latex 1_U$ goes). Note that both $latex C(-,U)$ and $latex [I,C](\Delta_-, D)$ are functors $latex C^{\text{op}} \to \mathbf{Set}$. The Yoneda lemma says that a natural transformation from $latex C(-,U) = H_U$ to $latex [I,C](\Delta_-, D)$ is isomorphic to $latex [I,C](\Delta_U, D)$ — i.e. a cone with vertex $latex U$, the universal cone.</p></li>
<li><p>The universality of this cone apparently comes from naturality.</p></li>
</ul>
<h3 id="general-limits-and-colimits-5">General limits and colimits 5</h3>
<p>http://youtu.be/Ud_k4HFIogQ</p>
<ul>
<li>Requires adjunctions.</li>
<li>Notation for limits. Categories that “have all limits (of a given shape)”.</li>
<li>The natural isomorphism defining a limit can be seen as an adjunction $latex \Delta \dashv \lim_{I \leftarrow}$ where $latex \Delta : C \to [I,C]$, and $latex \lim_{I \leftarrow} : [I,C] \to C$ is the functor that takes a diagram and produces its limit.</li>
<li>Claim: this is an adjunction <em>if</em> $latex C$ has all $latex I$-limits. Need to show that the iso is also natural in $latex D$, and that $latex \lim_{I  \leftarrow}$ is actually a functor.</li>
</ul>
<h3 id="general-limits-and-colimits-6">General limits and colimits 6</h3>
<p>http://youtu.be/9UOdrRF_pNc</p>
<p>Colimits using the same general formulation. “Just dualize everything”.</p>
<ul>
<li><p>Cocone (“cone under the diagram”) is an object with morphisms <em>from</em> the objects in the diagram such that everything commutes.</p></li>
<li><p>Universal cocone: for any other cocone, there is a unique morphism from the universal cocone <em>to</em> the other cone which makes everything commute. Note it has to go that direction since the universal cocone is supposed to be a “factor” of other cocones.</p></li>
<li><p>In Eugenia’s opinion the word “cocone” is stupid.</p></li>
<li><p>More generally: natural isomorphism between cocones and morphisms. $latex C(\lim_{\to I}, V) \cong [I,C](D, \Delta_V)$. Limits in $latex C$ are the same as colimits in $latex C^{op}$, and vice versa.</p></li>
<li><p>All limits are terminal objects in a category of cones (and colimits are initial objects).</p></li>
<li><p>Since terminal objects are initial objects in $latex C^{op}$ (and vice versa), we can even say that all universal properties are initial objects (and terminal objects) <em>somewhere</em>.</p></li>
</ul>
<h2 id="slice-and-comma-categories">Slice and comma categories</h2>
<h3 id="slice-and-comma-categories-1">Slice and comma categories 1</h3>
<p>https://www.youtube.com/watch?v=f4jpvwwnq_s</p>
<p>Slice category. Given a category $latex C$, fix an object $latex X \in C$. Then we define the slice category $latex C/X$ by</p>
<ul>
<li>Objects are pairs $latex (A,p)$ where $latex p : A \to X$.</li>
<li>Morphisms from $latex (A,p)$ to $latex (B,q)$ are morphisms $latex h : A \to B$ in $latex C$ which make the triangle commute.</li>
</ul>
<p>Coslice category, or “slice under” category $latex X/C$ is the dual of $latex C/X$, i.e. objects are pairs $latex (A,p)$ where $latex p : X \to A$, etc.</p>
<ul>
<li><p>If $latex C$ has a terminal object $latex 1$, $latex C/1 \cong C$. (Dually, $latex 0/C \cong C$.)</p></li>
<li><p>Products in $latex C/X$ are pullbacks in $latex C$ having $latex X$ as a corner. (Dually, coproucts in $latex X/C$ are pushouts.)</p></li>
</ul>
<h3 id="slice-and-comma-categories-2">Slice and comma categories 2</h3>
<p>http://youtu.be/W6sG5uraex0</p>
<p>Comma categories are a generalization of slice categories. Fix a functor $latex F : C \to D$ and an object $latex X \in D$. Then we can form the comma category $latex F \downarrow X$.</p>
<ul>
<li>Objects: pairs $latex (c \in C, p : F c \to X)$. Image of some object under $latex F$ and an arrow from it to $latex X$.</li>
<li>Morphisms are morphisms $latex f$ in $latex C$ such that $latex F f$ makes the relevant diagram commute.</li>
</ul>
<p>Of course we can dualize, $latex X \downarrow F$ (“cocomma” sounds even stupider than “cocone”, perhaps).</p>
<p>Apparently comma categories give us nice ways to talk about adjunctions.</p>
<p>Let’s generalize even more! Fix the functor $latex F$ but not the object $latex X \in D$. Then we can form $latex F \downarrow D$:</p>
<ul>
<li>Objects: triple $latex (c \in C, X \in D, p : F C \to X)$.</li>
<li>Morphism $latex (c, X, p) \to (c', X', p')$ is a pair of morphisms $latex c \to  c'$ and $latex X \to X'$ such that the relevant square commutes.</li>
</ul>
<p>Can also dualize, $latex D \downarrow F$.</p>
<p>An <em>even further</em> generalization! Start with two functors $latex F : C \to D$, $latex G : E \to D$. Form $latex F \downarrow G$:</p>
<ul>
<li>Objects: triples $latex (c \in C, e \in E, p : F c \to G e)$.</li>
<li>Morphisms: obvious generalization.</li>
</ul>
<p>In fact, all of these constructions are universal and can be seen as limits/colimits from the right point of view. “Next time”. (?)</p>
<h2 id="coequalisers">Coequalisers</h2>
<h3 id="coequalisers-1">Coequalisers 1</h3>
<p>https://youtu.be/vYktRTtulek</p>
<p>Coequalisers are a colimit. Show up all over the place. Give us quotients and equivalence relations. Also tell us about <em>monadicity</em> (given an adjunction, is it a monadic one?).</p>
<p>Definition: a <em>coequaliser</em> is a colimit of a diagram consisting of two parallel arrows.</p>
<p>More specifically, given $latex f,g : A \to B$, a coequaliser is an object $latex C$ equipped with $latex e : B \to C$ such that $latex f;e = g;e$, with a universal property: given any other $latex s : B \to V$ with $latex f;s = g;s$, $latex s$ factors uniquely through $latex e$.</p>
<ul>
<li><p>Example: in $latex \mathbf{Set}$: coequaliser of $latex f,g : A \to B$ is a quotient $latex B/\sim$, where $latex \sim$ is the equivalence relation generated by $latex f(a) \sim g(a)$ for all $latex a \in A$.</p></li>
<li><p>Conversely, we can <em>start</em> with an equivalence relation and build it using a coequaliser. Given: an equivalence relation $latex R \subseteq B  \times B$. Note we have $latex \pi_1, \pi_2 : R \to B$. Coequaliser is equivalence classes of $latex R$.</p></li>
</ul>
<h3 id="coequalisers-2">Coequalisers 2</h3>
<p>https://www.youtube.com/watch?v=DMSPS6us__Y</p>
<p>Quotient groups as coequalisers. Consider a group $latex G$ and a normal subgroup $latex H \triangleleft G$. In the category of groups, consider two parallel maps $latex H \to G$: the inclusion map $latex \iota$, and the zero map $latex 0$ which sends everything to the identity element $latex e \in G$. Claim: the coequaliser of these two maps is the quotient group $latex G/H$, together with the quotient map $latex G \to G/H$.</p>
<p>Let’s see why. Suppose we have another group $latex V$ with a group homomorphism $latex \theta : G \to V$ such that $latex \iota ; \theta = 0 ; \theta = 0$; that is, $latex \theta(h) = e$ for all $latex h \in H$. We must show there is a unique homomorphism $latex G/H \to V$ which makes the diagram commute.</p>
<p>Notation: $latex g \in G$ under the quotient map gets sent to $latex [g] = gH$ ($latex g_1 \sim g_2$ iff $latex g_2 g_1^{-1} \in H$). For the homomorphism $latex G/H \to V$, send $latex [g]$ to $latex \theta(g)$. Note this is required to make things commute, which gives us uniqueness; we must check this is well-defined and a group homomorphism. If $latex g_1 \sim g_2$ then $latex g_2 g_1^{-1} \in H$. By definition, $latex \theta(g_2 g_1^{-1}) = e$, and since $latex \theta$ is a group homomorphism, $latex \theta(g_2) = \theta(g_1)$. Hence it is well-defined, and must additionally be a group homomorphism since $latex [g_1] [g_2] = [g_1 g_2]$ and $latex \theta$ is a group homomorphism.</p>
<h2 id="monoid-objects">Monoid objects</h2>
<h3 id="monoid-objects-1">Monoid objects 1</h3>
<p>https://www.youtube.com/watch?v=PH-OhkrXXvA</p>
<p>Idea: take the definition of monoids from $latex \mathbf{Set}$, and “plunk it” into any other category with enough structure.</p>
<ul>
<li>A <em>monoid</em> is:
<ul>
<li>A set $latex A$</li>
<li>A binary operation $latex a \cdot b$ on $latex A$</li>
<li>A unit $latex e \in A$</li>
<li>Associativity: $latex \forall a,b,c \in A, (ab)c = a(bc)$</li>
<li>Identity: $latex \forall a, ea = a = ae$</li>
</ul></li>
</ul>
<p>Now let’s reexpress this categorically in $latex \mathbf{Set}$. Note we have been talking about <em>elements</em> of sets; we have to replace this with use of only objects and morphisms of $latex \mathbf{Set}$.</p>
<ul>
<li>A <em>monoid</em> (take 2) is:
<ul>
<li>An object $latex A \in \mathbf{Set}$</li>
<li>A morphism $latex \mu : A \times A \to A$ (note we use Cartesian product structure of $latex \mathbf{Set}$)</li>
<li>A morphism $latex \eta : 1 \to A$</li>
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
<li>A <em>monoid object</em> in a monoidal category $latex C$ is:
<ul>
<li>An object $latex A \in C$</li>
<li>A morphism $latex \mu : A \otimes A \to A$</li>
<li>A morphism $latex \eta : 1 \to A$</li>
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
<p>Note first the category of monoids is itself monoidal under Cartesian product. That is, given two monoids $latex M$ and $latex N$, $latex M \times N$ is also a monoid.</p>
<p>Now, what is a monoid object in $latex \mathbf{Mon}$?</p>
<ul>
<li>An object $latex M \in \mathbf{Mon}$, <em>i.e.</em> a monoid</li>
<li>Monoid morphisms $latex \eta : 1 \to M$ and $latex \mu : M \times M \to M$</li>
<li>…satisfying unit and associativity laws.</li>
</ul>
<p>$latex \eta : 1 \to M$ is a <em>monoid</em> morphism so it has to send the single object of $latex 1$ to the unit of $latex M$. Hence $latex \eta$ is entirely constrained and uninteresting.</p>
<p>$latex \mu : M \times M \to M$ has to be a monoid map. That is, $latex \mu((a,b) \circ (c,d)) = \mu(a,b) \circ \mu(c,d)$, <em>i.e.</em> $latex \mu(a \circ c, b \circ d) = \mu(a,b) \circ \mu(c,d)$. So $latex \mu$ has to “commute” with $latex \circ$. This is precisely the condition needed to apply Eckmann-Hilton.</p>
<p>Monoid object is also required to satisfy unital and associativity laws, but we can already deduce those from Eckmann-Hilton.</p>
<h2 id="categories">2-categories</h2>
<h3 id="categories-1">2-categories 1</h3>
<p>https://www.youtube.com/watch?v=k-RehY4tLdI</p>
<p>Generalization of categories: not just objects and morphisms, but also (2-)morphisms between the (1-)morphisms. Primordial example: categories, functors, and natural transformations.</p>
<p>Note: today, <em>strict</em> 2-categories, i.e. everything will hold on the nose rather than up to isomorphism. A bit immoral of us. [If we let things be a bit looser we get bicategories?]</p>
<p>Recall: a (small) <em>category</em> $latex C$ is given by</p>
<ul>
<li>A set $latex \mathrm{Ob}\ C$ of objects</li>
<li>for all $latex x,y \in \mathrm{Ob}\ C$, a set $latex C(x,y)$ of morphisms</li>
</ul>
<p>equipped with</p>
<ul>
<li>identities: for all $latex x \in C$ a function $latex 1 \to C(x,x)$</li>
<li>composition: for all $latex x,y,z \in C$, a composition function $latex C(y,z)  \times C(x,y) \to C(x,z)$.</li>
<li>unit and associativity laws.</li>
</ul>
<p>To make this into a 2-category, we take the <em>set</em> of morphisms and <em>categorify</em> it. That turns some of the above <em>functions</em> into <em>functors</em>. Thus, a $latex 2$-category $latex C$ is given by a set of objects along with</p>
<ul>
<li>a <em>category</em> $latex C(x,y)$ for each $latex x,y \in C$</li>
<li>a <em>functor</em> $latex 1 \to C(x,x)$ for each $latex x \in C$</li>
<li>a composition <em>functor</em> $latex C(y,z) \times C(x,y) \to C(x,z)$.</li>
<li>etc.</li>
</ul>
<p>(Note: why not turn the set of <em>objects</em> into a category? That’s a good question. Turns out we would get something different.)</p>
<p>Let’s unravel this a bit. If $latex C(x,y)$ is a <em>category</em> then the objects are morphisms (of $latex C$) $latex x \to y$, and there can also be morphisms (of $latex C(x,y)$) <em>between</em> these morphisms: $latex 2$-cells. $latex 2$-cells can be composed (“vertical” composition).</p>
<p>We also have the composition functor $latex C(y,z) \times C(x,y) \to C(x,z)$. On “objects” (which are $latex 1$-cells in $latex C$) the action of this functor is just the usual composition of $latex 1$-cells. On morphisms (i.e. $latex 2$-cells), it gives us “horiztonal” composition.</p>
<p>Next time: how functoriality gives us the interchange law.</p>
<h3 id="categories-2">2-categories 2</h3>
<p>http://youtu.be/DRGh-HESyag</p>
<p>Interchange in a 2-category comes from functoriality of the composition functor. The key is to remain calm.</p>
<p>The functor is $latex C(y,z) \times C(x,y) \to C(x,z)$. On morphisms, it sends pairs of $latex 2$-cells to a single $latex 2$-cell, the horizontal composite. What does functoriality mean? It means if we have two (vertically!) composable pairs of $latex 2$-cells; the functor on their composition (<em>i.e</em> doing vertical composition pointwise) is the same as applying the functor to each (<em>i.e.</em> first doing the horizontal compositions) and then composing (vertically).</p>
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
<p>Given a set with two binary operations $latex \circ$ and $latex \star$, such that</p>
<ul>
<li>$latex \circ$ and $latex \star$ are unital, with the same unit;</li>
<li>one of them distributes over the other, i.e. $latex (a \star b) \circ (c  \star d) = (a \circ c) \star (b \circ d)$,</li>
</ul>
<p>then $latex \circ = \star$, and the operation is commutative.</p>
<p>Geometric intuition: $latex \circ$ and $latex \star$ could be vertical and horizontal composition of $latex 2$-cells in a bicategory. Then distributivity is just the interchange law.</p>
<p>Proof: use the “Eckmann-Hilton clock”. See video for pictures. Given e.g. $latex a \circ b$, “rotate” $latex a$ and $latex b$ around each other by inserting units and using interchange law.</p>
<p>In fact, it is not necessary to require that the two units are the same: it is implied by the interchange law. Left as an exercise.</p>
<h3 id="eckmann-hilton-2">Eckmann-Hilton 2</h3>
<p>https://www.youtube.com/watch?v=wnRqo7UHa-k</p>
<p>This time, show the interchange law implies the units are the same and associativity.</p>
<p>Let $latex v$ be the vertical unit and $latex h$ the horizontal unit. Then $latex ((v \circ h) \star (h \circ v)) = h \star h = h$ but also by interchange law it is equal to $latex ((v \star h) \circ (h \star v)) = v \circ v = v$, hence $latex h = v$.</p>
<p>$latex (a \circ 1) \star (b \circ c) = a \star (b \circ c)$; interchange gives $latex (a \star b) \circ (1 \star c) = (a \star b) \circ c$. Since the two operations have to be the same, this gives associativity.</p>
<p>Example. A (small) $latex 2$-category with only one $latex 0$-cell and only one $latex 1$-cell is in fact a commutative monoid. Underlying set is set of $latex 2$-cells. Operation is either $latex \circ$ or $latex \star$, which by Eckmann-Hilton are the same and commutative.</p>
<p>Bicategory case is a bit more complicated, since horizontal composition is not strictly unital. A bicategory with only one $latex 0$-cell is a monoidal category. A bicategory with only one $latex 1$-cell is a commutative monoid.</p>
<h2 id="distributive-laws">Distributive laws</h2>
<h3 id="distributive-laws-1">Distributive laws 1</h3>
<p>https://www.youtube.com/watch?v=mw4IhOLhDwY</p>
<p>Monads represent algebraic structure; a distributive law says when two algebraic structures interact with each other in a coherent way. Motivating example: multiplication and addition in a ring.</p>
<p>Let $latex S$, $latex T$ be monads on a category $latex C$. A <em>distributive law</em> of $latex S$ over $latex T$ is a natural transformation $latex \lambda : ST \to TS$, satisfying the “obvious” axioms: $latex \lambda$ needs to interact properly with the monad structure of $latex S$ and $latex T$, that is:</p>
<ul>
<li>$latex \eta_S T ; \lambda = T \eta_S$</li>
<li><p>$latex S \lambda ; \lambda S ; T \mu_S = \mu_S T ; \lambda$</p></li>
<li>$latex S \eta_T ; \lambda = \eta_T S$</li>
<li><p>$latex \lambda T ; T \lambda ; \mu_T S = S \mu_T ; \lambda$</p></li>
</ul>
<p>Example: $latex C = \mathbf{Set}$. $latex S$ = free commutative monoid monad (“multiplication”), $latex T$ = free abelian group monad (“addition”). Define $latex \lambda_X : STX \to TSX$: $latex TX$ is formal sums of elements of $latex X$, like $latex (a + b + c)$; $latex S$ constructs formal products. So we have to send things like $latex (a + b)(c + d)$ to formal sums of formal products, $latex ac + bc + ad + bd$.</p>
<p>In fact we have constructed the free ring monad, $latex TS$.</p>
<p>If we start with a monoid and consider the free group on its underlying elements, we can define a product using distributivity; so the free group on a monoid is a group. Formally, the free group monad lifts to the category of monoids (?).</p>
<h3 id="distributive-laws-2">Distributive laws 2</h3>
<p>https://www.youtube.com/watch?v=TLgjH9Y8HOc</p>
<p>More abstract story behind our favorite example: combining a group and a monoid to get a ring.</p>
<p>Note: distributive law (at least in this example) is definitely non-invertible: you can turn a product of sums into a sum of products, but you can’t necessarily go in the other direction.</p>
<p>Main result: A distributive law $latex \lambda : ST \to TS$ is equivalent to a lift of $latex T$ to a monad $latex T'$ on $latex S$-$latex \mathbf{Alg}$. $latex TS$ becomes a monad, and $latex TS$-$latex \mathbf{Alg}$ is equivalent to $latex T'$-$latex \mathbf{Alg}$.</p>
<p>When is $latex TS$ a monad? We need $latex \mu : TSTS \to TS$; can do this if we have $latex \lambda : ST \to TS$, then use $latex \mu_T \mu_S$. The laws for a distributive law ensure that this satisfies the monad laws.</p>
<p>Distributive law is equivalent to a lift of $latex T$ to a monad on $latex S$-$latex \mathbf{Alg}$?</p>
<ul>
<li>An $latex S$-algebra looks like $latex \theta : SX \to X$; we want $latex T'$ to send this to another $latex S$-algebra, with carrier $latex TX$, i.e. some $latex \phi :  STX \to TX$. But we have $latex T \theta : TSX \to TX$; precomposing with $latex \lambda$ gives us what we want, and the distributive law axioms ensure that $latex T'$ is a monad on $latex S$-$latex \mathbf{Alg}$.</li>
</ul>
<p>$latex TS$-$latex \mathbf{Alg}$ is equivalent to $latex T'$-$latex \mathbf{Alg}$?</p>
<ul>
<li><p>Since $latex T'$ is a monad on $latex \mathbf{Alg} S$, a $latex T'$-algebra has <em>an $latex S$-algebra</em> as its underlying object. So given some $latex S$-algebra $latex \theta : SX \to X$, a $latex T$’-algebra on it is a morphism of $latex S$-algebras from $latex T'\theta$ to $latex \theta$, that is,</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/kzpuvz4.png" />

</div>
</div>
<p>This essentially says that an algebra for $latex T'$ is simultaneously an algebra for $latex S$ and an algebra for $latex T$ which interact properly via the distributivity law.</p></li>
<li><p>An algebra for $latex TS$ is $latex \psi : TSX \to X$. Clear that from a $latex T'$-algebra we get a $latex TS$ algebra. What about vice versa? Just precompose $latex \psi$ with $latex \eta_T$ to get an $latex S$-algebra, and with $latex T  \eta_S$ to get a $latex T$-algebra. A $latex TS$-algebra says how to evaluate e.g. multiplication and additions all mixed up; precomposing with $latex \eta$ picks out the stuff with just multiplication or just addition. Apparently one can prove that the algebras you get this way do indeed interact nicely.</p></li>
</ul>
<h3 id="distributive-laws-3-aka-monads-6">Distributive laws 3 (aka Monads 6)</h3>
<p>https://www.youtube.com/watch?v=g-SCYArh5RY</p>
<p>Recall that a monad is a functor together with some natural transformations; we can see this as a construction in the $latex 2$-category of categories, functors, and natural transformations. We can carry out the same construction in any $latex 2$-category $latex C$, giving monads <em>in</em> $latex C$.</p>
<p>Let $latex C$ be a $latex 2$-category (<em>e.g.</em> $latex \mathbf{Cat}$). A monad <em>in</em> $latex C$ is given by</p>
<ul>
<li>a $latex 0$-cell $latex X$</li>
<li>a $latex 1$-cell $latex T : X \to X$</li>
<li>a pair of $latex 2$-cells $latex \eta : 1 \to T$ and $latex \mu : T^2 \to T$</li>
</ul>
<p>satisfying the usual monad axioms.</p>
<p>In fact, we get an entire $latex 2$-category of monads inside $latex C$!</p>
<p>What is a morphism of monads? A monad functor $latex (X_1, S_1, \eta_1, \mu_1) \to (X_2, S_2, \eta_2, \mu_2)$ (<em>i.e.</em> a $latex 1$-cell in the $latex 2$-category of monads in $latex C$) is given by</p>
<ul>
<li>a $latex 1$-cell $latex u : X_1 \to X_2$</li>
<li>a $latex 2$-cell $latex \lambda : S_2 u \to u S_1$ (Note, this is not backwards! This is what we will need to map algebras of the firs monad to algebras of the second.)</li>
</ul>
<p>satisfying the axioms:</p>
<ul>
<li>$latex \eta_{S_2} u ; \lambda = u \eta_{S_1}$</li>
<li>$latex S_2 \lambda ; \lambda S_1 ; u \mu_{S_1} = \mu_{S_2} ; \lambda$</li>
</ul>
<p>A monad transformation (<em>i.e.</em> a $latex 2$-cell in the $latex 2$-category of monads in $latex C$) is given by</p>
<ul>
<li>a $latex 2$-cell $latex \sigma : u_1 \to u_2$, satisfying $latex \sigma S_2 ;  \lambda_2 = \lambda_1 ; S_1 \sigma$ (something like that, see pasting diagrams of $latex 2$-cells in the video).</li>
</ul>
<h3 id="distributive-laws-4">Distributive laws 4</h3>
<p>https://www.youtube.com/watch?v=FZeoHPRoBVk</p>
<p>Distributive laws, even more formally!</p>
<p>Consider the $latex 2$-category of monads $latex \mathbf{Mnd}(C)$ in an arbitrary $latex 2$-category $latex C$; monads <em>in</em> $latex \mathbf{Mnd}(C)$ are distributive laws!</p>
<p>Recall that a monad in an arbitrary $latex 2$-category is a $latex 0$-cell equipped with an endo-$latex 1$-cell and appropriate $latex 2$-cells $latex \eta$ and $latex \mu$. In $latex \mathbf{Mnd}(C)$:</p>
<ul>
<li>A $latex 0$-cell in $latex \mathbf{Mnd}(C)$, that is, a monad in $latex C$ $latex (X,S)$.</li>
<li>A $latex 1$-cell $latex (X,S) \to (X,S)$, that is, a functor $latex T : X \to X$ and $latex \lambda : ST \to TS$.</li>
<li>$latex 2$-cells $latex \eta$ and $latex \mu$. Can check that these turn $latex T$ into a monad.</li>
<li>Axioms on $latex \lambda$ give exactly what is needed to make it a distributive law.</li>
</ul>
<p>Summarizing more concisely/informally, a monad in $latex \mathbf{Mnd}(C)$ is</p>
<ul>
<li>A $latex 0$-cell $latex X$</li>
<li>A pair of monads $latex S$,$latex T$</li>
<li>A distributive law $latex \lambda : ST \to TS$.</li>
</ul>
<p>Consider the map $latex C \mapsto \mathbf{Mnd}(C)$. This actually defines an endofunctor $latex \mathbf{Mnd}(-)$ on $latex 2$-$latex \mathbf{Cat}$, the category of (strict) $latex 2$-categories and (strict) $latex 2$-functors. In fact, Street showed that $latex \mathbf{Mnd}(-)$ is a monad! The “monad monad”.</p>
<p>The multiplication has type $latex \mathbf{Mnd}(\mathbf{Mnd}(C)) \to \mathbf{Mnd}(C)$. Recall that objects in $latex \mathbf{Mnd}(\mathbf{Mnd}(C))$ are a pair of monads $latex S$,$latex T$ plus a distributive law. In fact, the distributive law is precisely what is needed to make $latex TS$ into a monad, which is the monad returned by the multiplication.</p>
<h2 id="group-objects-and-hopf-algebras">Group Objects and Hopf Algebras</h2>
<h3 id="group-objects-and-hopf-algebras-1">Group Objects and Hopf Algebras 1</h3>
<p>https://www.youtube.com/watch?v=p3kkm5dYH-w</p>
<p>Take the idea of a group and develop it categorically, first in the category of sets and then transport it into other categories (though it may not be completely obvious what properties of $latex \mathbf{Set}$ we are using).</p>
<p>A group is of course a set $latex G$ with an associative binary product, inverses, and an identity element. Let’s make this categorical: don’t want to talk about internal structure of $latex G$ but just about $latex G$ as an object in $latex \mathbf{Set}$.</p>
<p>So a group is:</p>
<ul>
<li>an object $latex G \in \mathbf{Set}$</li>
<li>a multiplication morphism $latex \mu : G \times G \to G$</li>
<li>an inverse morphism $latex \gamma : G \to G$</li>
<li>a unit morphism $latex \eta : 1 \to G$ (<em>i.e.</em> “universal element”)</li>
</ul>
<p>together with axioms expressed as commutative diagrams:</p>
<ul>
<li>$latex (id \times \mu) ; \mu = (\mu \times id) ; \mu$</li>
<li>$latex (\eta \times id) ; \mu = id = (id \times \eta) ; \mu$ (note to be pedantic we also need to use $latex \lambda : G \to 1 \times  G$ and $latex \rho : G \to G \times 1$)</li>
<li>$latex \Delta ; (id \times \gamma) ; \mu = \varepsilon ; \eta$</li>
</ul>
<p>where $latex \Delta : G \to G \times G$ is the diagonal map (note the fact that we are using $latex \Delta$ is the most interesting part; see forthcoming lectures) and $latex \varepsilon : G \to 1$ is the unique map to a terminal set.</p>
<h3 id="group-objects-and-hopf-algebras-2">Group Objects and Hopf Algebras 2</h3>
<p>https://www.youtube.com/watch?v=kJ2X_U7X5WA</p>
<p>Note just $latex \mu$ and $latex \eta$ together with axioms (forgetting about $latex \gamma$ and its axioms) is the definition of a monoidal category. Not surprising since a group is a monoid with inverses.</p>
<p>Recall $latex \Delta : G \to G \times G$. We get that for free from the fact that the monoid we are using is really the categorical product; $latex \Delta$ can be easily defined using the universal property of categorical product.</p>
<p>In fact, every set $latex S$ is a <em>comonoid</em> in a unique way, since $latex \times$ is a categorical product. That is, a comonoid on a set $latex S$ is given by</p>
<ul>
<li>a comultiplication $latex \Delta : S \to S \times S$</li>
<li>a counit $latex \varepsilon : S \to 1$</li>
<li>satisfying coassociativity and counit axioms.</li>
</ul>
<p>And note we used $latex \Delta$ and $latex \varepsilon$ in the definition of a group, in particular in the axioms for $latex \gamma$.</p>
<h3 id="group-objects-and-hopf-algebras-3">Group Objects and Hopf Algebras 3</h3>
<p>https://www.youtube.com/watch?v=wAeHrtKMTHM</p>
<p>The definition given last time won’t work in general for any monoidal category, but it does work for any <em>Cartesian</em> category (that is, monoidal categories where the monoidal operation is categorical product). Examples of Cartesian categories, in which it therefore makes sense to have group objects, include:</p>
<ul>
<li>$latex \mathbf{Set}$</li>
<li>$latex \mathbf{Top}$ (category of topological spaces, with Cartesian product toplogy)</li>
<li>$latex \mathbf{Diff}$ (cat. of smooth manifolds?)</li>
<li>$latex \mathbf{Grp}$ (groups)</li>
<li>$latex \mathbf{Cat}$ (categories)</li>
</ul>
<p>Let’s see what a group object looks like in each of these examples.</p>
<ul>
<li>In $latex \mathbf{Set}$, a group object is a group.</li>
<li>In $latex \mathbf{Top}$, a topological group.</li>
<li>In $latex \mathbf{Diff}$, a Lie group.</li>
<li>In $latex \mathbf{Grp}$, it turns out a group object is an Abelian group! (Details left as an exercise.)</li>
<li>In $latex \mathbf{Cat}$, we get a “crossed module”.</li>
</ul>
<p>What about non-Cartesian monoidal categories? Simplest example is $latex \mathbf{Vect}$, category of (finite-dimensional) vector spaces with linear maps. Monoidal structure given by tensor product and complex numbers. Tensor product defined by</p>
<p>$latex V \otimes W = \{ \sum_t \alpha_t (v_t \otimes w_t) \mid v_t \in V, w\_t \in W \} / [(\alpha_1 v_1 + \alpha_2 v_2) \otimes w \sim \alpha_1 (v_1 \otimes w) + \alpha_2(v_2 \otimes w) \text{ and symmetrically}]$</p>
<p>Suppose $latex \{v_i\}$ is a basis for $latex V$ and $latex \{w_j\}$ is a basis for $latex W$, then $latex \{v_i \otimes w_j\}$ is a basis for $latex V \otimes W$.</p>
<p>The point is that $latex \mathrm{dim}(V \otimes W) = \mathrm{dim}(V) \times \mathrm{dim}(W)$, but that’s different than $latex \mathrm{dim}(V \times W) = \mathrm{dim}(V) + \mathrm{dim}(W)$, so $latex \mathbf{Vect}$ is <em>not</em> Cartesian.</p>
<h3 id="group-objects-and-hopf-algebras-4">Group Objects and Hopf Algebras 4</h3>
<p>https://www.youtube.com/watch?v=zZn9ZETVkF8</p>
<p>We still want to be able to define group objects in monoidal categories which are not Cartesian.</p>
<p>Recall: if we have a monoidal category $latex (C, \times, 1)$ where $latex \times$ is the categorical product, then every object $latex X \in C$ is a comonoid $latex (X, \Delta, \varepsilon)$ in a unique way, and every morphism is a comonoid map.</p>
<p>Notation: in $latex \mathbf{Set}$, an object with an associative binary operation and an identity is called a <em>monoid</em>; in $latex \mathbf{Vec}$ it’s called an <em>algebra</em>. So when we generalize to arbitrary categories sometimes “monoid” is used, sometimes “algebra”.</p>
<p>A <em>Hopf algebra</em> is a group object in a general monoidal (tensor) category. Details next time.</p>
<h3 id="group-objects-and-hopf-algebras-5">Group Objects and Hopf Algebras 5</h3>
<p>https://www.youtube.com/watch?v=gmxZ_KCRZho</p>
<p>A <em>Hopf algebra</em> $latex H$ in a (braided) monoidal category is as follows. We don’t get comonoid stuff for free any more so we have to put it in “by hand”.</p>
<ul>
<li>comonoid $latex \Delta : H \to H \otimes H$ and $latex \varepsilon : H \to 1$</li>
<li>monoid $latex \mu : H \otimes H \to H$ and $latex \eta : 1 \to H$</li>
<li>“antipode” or inverse $latex \gamma : H \to H$</li>
</ul>
<p>(See video for string diagrams.) Note the monoid and comonoid also need to be “compatible”: this is where the braidedness comes in. In particular $latex \mu$ and $latex \eta$ need to be <em>comonoid morphisms</em>. So we need $latex H \otimes H$ to be a coalgebra.</p>
<p>Lemma: suppose $latex H$, $latex K$ are comonoids. Then $latex H \otimes K$ is a coalgebra if the category is braided: $latex H \otimes K \to (H \otimes H) \otimes (K \otimes K)$ using comonoid structures on $latex H$ and $latex K$, and then using (associativity and) braiding we can flip inner $latex H \otimes K$ around to get $latex (H \otimes K) \otimes (H \otimes K)$.</p>
<p>Can then write down what it means for $latex \mu$ to be a coalgebra map aka comonoid morphism; left as an exercise (or the next video).</p>
<h3 id="group-objects-and-hopf-algebras-6">Group Objects and Hopf Algebras 6</h3>
<p>https://www.youtube.com/watch?v=Gv1sRLOwVWA</p>
<p>String diagram showing comonoid $latex \Delta$ for $latex H \otimes K$.</p>
<p>$latex \mu$ and $latex \eta$ should be a comonoid morphism, <em>i.e.</em> must commute with $latex \Delta$ (string diagram) and also with $latex \varepsilon$ (another string diagram).</p>
<p>There seems to be some asymmetry: monoid + comonoid + monoid must be comonoid morphisms. But it’s the same to say that the comonoid must be monoid morphisms.</p>
<h2 id="ends">Ends</h2>
<h3 id="ends-1">Ends 1</h3>
<p>http://youtu.be/mxI9ba6Rexc</p>
<p>Given a functor $latex T : C^{op}\times C \to D$, an <em>end</em> $latex \int_{c \in C} T(c,c)$ is an object in $latex D$ which is “limit-like” in some sense.</p>
<p>Ends are not as common as coends (and perhaps not as intuitive?). Two particular places where ends do show up:</p>
<ul>
<li>natural transformations (especially in enriched setting; see Ends 2)</li>
<li>reconstruction theorems (recover an algebra from category of its representations, <em>i.e.</em> <em>Tannaka reconstruction</em>, see Ends 3)</li>
</ul>
<p>Definition:</p>
<ul>
<li>A <em>wedge</em> $latex x \stackrel{\bullet}{\to} T$ consists of
<ul>
<li>an object $latex x \in D$</li>
<li>a family of $latex D$-morphisms $latex \omega_c : x \to T(c,c)$ for all $latex c \in C$</li>
<li>such that for all $latex f : c \to c'$ the obvious square with vertices $latex x$, $latex T(c,c)$, $latex T(c',c')$, and $latex T(c,c')$ commutes. (Dinaturality/extranaturality.)</li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/mrpq76b.png" />

</div>
</div>
<ul>
<li>This is in some sense a generalization of a cone.</li>
</ul></li>
<li><p>An <em>end</em> is a universal wedge, <em>i.e.</em> a wedge $latex E  \stackrel{\bullet}{\to} T$ such that if $latex x \stackrel{\bullet}{\to}  T$ then there exists a unique morphism $latex x \to E$ through which the components of $latex x$ factor.</p></li>
</ul>
<p>Note we write the object $latex E$ using the intergral notation, $latex \int_{c \in C} T(c,c)$ (the morphisms of the wedge are left implicit).</p>
<h3 id="ends-2">Ends 2</h3>
<p>http://youtu.be/gyc86NFT0Sw</p>
<p>Simple example of an end: $latex T = \mathrm{Hom} : C^{\text{op}} \times C  \to \mathbf{Set}$. In this case a wedge $latex x \stackrel{\bullet}{\to}  T$ consists of:</p>
<ul>
<li>some $latex x \in \mathbf{Set}$</li>
<li>for each $latex c \in C$ a function $latex \omega_c : x \to  \mathrm{Hom}(c,c)$</li>
<li>such that $latex \forall f : c \to c'$ we have $latex \omega_c; f \circ - =  \omega_{c'}; - \circ f$.</li>
</ul>
<p>That is, for every $latex n \in x$ we have $latex \omega_c(n) = n_c : c \to c$, such that $latex f \circ n_c = c_{c'} \circ f$. <em>i.e.</em> the family $latex n_c$ are the components of a natural transformation $latex Id_C \to Id_C$.</p>
<p>Note this goes in the other direction too, that is, a wedge $latex x \stackrel{\bullet}{\to} \mathrm{Hom}$ is precisely the same thing as a function $latex x \to \mathrm{Nat}(Id_C, Id_C)$. Therefore, the universal such $latex x$ is precisely this set of natural transformations. (Can be thought of as “set of symmetries” of a category. Also the <em>Hochschild cohomology</em>.)</p>
<h3 id="ends-3">Ends 3</h3>
<p>http://youtu.be/TfSUxhCNZZ0</p>
<p>More examples. First, straightforward generalization: given functors $latex F, G : C \to E$, form the bifunctor $latex \mathrm{Hom}_E(F(-), G(-)) : C^{op}\times C \to \mathbf{Set}$. Then we can see that</p>
<p>$latex \int_{c \in C} \mathrm{Hom}_E(F(c),G(c)) = \mathrm{Nat}(F,G)$.</p>
<p>(Proof is just a small generalization of the proof in Ends 2, left as exercise.) Useful in an enriched context, can use this end to construct an <em>object</em> of natural transformations instead of a set.</p>
<p>Another example, “baby Tannaka reconstruction” (see <a href="http://ncatlab.org/nlab/show/Tannaka+duality">Tannaka duality</a> and <a href="http://ncatlab.org/nlab/show/reconstruction+theorem">reconstruction theorem</a> on nlab).</p>
<ul>
<li>$latex M$ is a monoid in the category of sets.</li>
<li>$latex M$-$latex \mathbf{Set}$ is the category of sets that $latex M$ acts on</li>
<li>$latex U : M\text{-}\mathbf{Set} \to \mathbf{Set}$ is the forgetful functor.</li>
<li>Result: $latex \int_{(S,\sigma) \in M\text{-}\mathbf{Set}} \mathrm{Hom}  (U(S,\sigma), U(S,\sigma)) \cong \mathrm{Nat}(U,U) \cong M$. (In general, natural transformations over forgetful functor reconstructs algebraic objects.)</li>
</ul>
<p>Proof (application of Yoneda):</p>
<ul>
<li>Let $latex \mu : M \times M \to M$ be monoid operation and $latex e$ the identity.</li>
<li>An $latex M$-set consists of a set $latex S$ together with an action $latex \sigma : M  \times S \to S$. Morphisms $latex (S,\sigma) \to (R,\rho)$ are just functions $latex S \to R$ which commute with the actions (“equivariant maps”).</li>
<li>Note $latex U$ is representable:
<ul>
<li>Consider the $latex M$-set $latex M^l = (M, \mu)$ (“left-regular representation of $latex M$”)</li>
<li>Define $latex \mathrm{Hom}(M^l, (S, \sigma)) \to  S$ by $latex f \mapsto f(e)$.</li>
<li>Note $latex f(e)$ determines $latex f$ since $latex f$ is equivariant: $latex f(m) = f(m  \cdot e) = m \cdot f(e)$. In fact $latex \mathrm{Hom}(M^l, (S,  \sigma)) \cong S$.</li>
<li>Thus $latex U = \mathrm{Hom}(M^l, -)$.</li>
</ul></li>
<li>So $latex \mathrm{Nat}(U,U) = \mathrm{Nat}(\mathrm{Hom}(M^l,-),  \mathrm{Hom}(M^l,-))$, and by (co-?)Yoneda, this is just $latex \mathrm{Hom}(M^l, M^l) = U(M^l) = M$.</li>
</ul>
<h3 id="ends-4">Ends 4</h3>
<p>http://youtu.be/3hOtm0paWXY</p>
<p>Combine some of the previous examples. Recall</p>
<ul>
<li>$latex \int_C \mathrm{Hom} \cong \mathrm{Nat}(Id,Id)$ (Ends 2)</li>
<li>$latex \int_{M\text{-}\mathbf{Set}} \mathrm{Hom}_{\mathbf{Set}}(U(-),  U(-)) \cong M$ (Ends 3)</li>
</ul>
<p>What happens if we combine these two results? First, look at the end from last time:</p>
<ul>
<li>Let $latex \Phi \in \int_{M\text{-}\mathbf{Set}}  \mathrm{Hom}_{M\text{-}\mathbf{Set}}(U -,U -) = \mathrm{Nat}(U,U)$ be a natural transformation on $latex U$. That is, $latex \Phi_{(R,\rho)}$ is a function $latex R \to R$, such that $latex \Phi$ commutes with the underlying function of any equivariant map, <em>i.e.</em></li>
</ul>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/qzxv7gq.png" />

</div>
</div>
<ul>
<li>As we showed last time, $latex \Phi_{(R,\rho)} = \rho(m)$ for some $latex m \in  M$.</li>
<li>Note $latex \Phi_{(R,\rho)}$ is just a function $latex R \to R$, but has to commute with equivariant functions.</li>
</ul>
<p>Now look at the end of the bare hom-functor in the category of $latex M$-sets. <em>i.e.</em> $latex \int_{M\text{-}\mathbf{Set}} \mathrm{Hom}_{M\text{-}\mathbf{Set}}(-,-) = ?$</p>
<ul>
<li><p>Now if $latex \Theta \in \int_{M\text{-}\mathbf{Set}} \mathrm{Hom}_{M\text{-}\mathbf{Set}}(-,-)$, we have</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://mathurl.com/mal2e6h.png" />

</div>
</div></li>
<li><p>What’s the difference? $latex \Theta$ is now a family of <em>equivariant</em> maps. But note equivariant maps are determined by their underlying function. So any diagram of this form implies one of the previous form; the only thing we’ve added is that $latex \Theta$ itself has to be equivariant (in the previous case $latex \Phi$ could be any function). So in fact we have</p>
<p>$latex \int \mathrm{Hom}(-,-) \subseteq \int \mathrm{Hom}(U -, U -) = M$.</p>
<p><em>i.e.</em> we’re picking out some subset of $latex M$. Question: <em>which</em> subset is it? That is, given such a $latex \Theta_{(R,\rho)}$ we know $latex \Theta_{(R,\rho)} = \rho(m)$ for some $latex m$; which $latex m$’s can we get?</p></li>
<li><p>Consider the left-regular representation $latex M^l$ again. Then we know $latex \Phi_{M^l} : M^l \to M_l$ is just left-multiplication by some $latex m  \in M$. But it has to commute with equivariant maps; picking the action on the particular element $latex e$, this means for all $latex n \in M$</p>
<p>$latex n \cdot \Phi_{M^l}(e) = \Phi_{M^l}(n \cdot e)$</p>
<p>that is, $latex nm = mn$, <em>i.e.</em> $latex m \in Z(M)$.</p></li>
<li><p>So we conclude $latex \int_{M\text{-}\mathbf{Set}}  \mathrm{Hom}_{M\text{-}\mathbf{Set}}(-,-) = Z(M)$.</p></li>
</ul>
<h2 id="adjunctions-from-morphisms">Adjunctions from morphisms</h2>
<h3 id="adjunctions-from-morphisms-1">Adjunctions from morphisms 1</h3>
<p>https://www.youtube.com/watch?v=SzzHjpRmrLU</p>
<p>General phenomenon: associate some category $latex C(X)$ to an object $latex X$. For example:</p>
<ul>
<li>In representation theory, to a group or algebra we associate a category of modules or representations.</li>
<li>In algebraic topology, to a space $latex X$ we associate $latex \mathbf{Vect}(X)$ (category of “bundles”?)</li>
<li>In algebraic geometry, to an algebraic variety associate the category of sheaves.</li>
<li>In logic, to a set of terms associate a category of subsets (predicates) over the terms.</li>
<li>In analysis, to a metric space $latex X$ associate a category of Lipschitz functions $latex X \to \mathbb{R}$.</li>
</ul>
<p>Question: if we have a morphism $latex f : X \to Y$, how does that relate to the categories $latex C(X)$ and $latex C(Y)$ associated to $latex X$ and $latex Y$?</p>
<p>We often get some sort of “pullback” functor $latex f^* : C(X) \leftarrow C(Y)$. (Also often get some sort of monoidal structure on $latex C(X)$ and $latex C(Y)$, and $latex f^*$ is often monoidal.)</p>
<p>We also get various “pushforwards” $latex f_* : C(X) \to C(Y)$, right adjoint to $latex f^*$. In some situation we also get a left adjoint to $latex f^*$.</p>
<p>This is the beginning of the story of “Grothendieck’s 6 operations”. Lots of similar structure arises in all these different areas.</p>
<h3 id="adjunctions-from-morphisms-2">Adjunctions from morphisms 2</h3>
<p>https://www.youtube.com/watch?v=jAQfNGEOass</p>
<p>Baby examples of some particular adjunctions (in generality, they show up in Grothendieck’s 6 operations, Frobenius reciprocity, …). Idea: start with (<em>e.g.</em>) sets; to each set associate a category; to each morphism between sets we will get functors between the categories.</p>
<ul>
<li>To the set $latex X$ associate the slice category $latex \mathbf{Set}/X$.
<ul>
<li>Think of the objects of this slice category, $latex E \to X$, as “bundles over $latex X$”: a base space $latex X$ and a set above that, where each element of $latex X$ is associated to its fiber/preimage.</li>
<li>Another way to think of this is as a functor $latex \hat{E} : X \to  \mathbf{Set}$ (considering $latex X$ as a discrete category), that picks out the fiber of each element of $latex X$.</li>
</ul></li>
<li><p>There is actually an equivalence of categories $latex \mathbf{Set}^X \cong  \mathbf{Set}/X$.</p></li>
<li><p>What about maps between sets? <em>e.g.</em> $latex f : X \to Y$. As we’ll see, we get three associated maps $latex f^* : \mathbf{Set}^Y \to  \mathbf{Set}^X$, $latex f_* : \mathbf{Set}^X \to \mathbf{Set}^Y$, and $latex f_! : \mathbf{Set}^X \to \mathbf{Set}^Y$, with $latex f_! \dashv f^*  \dashv f_*$. Details in the next lecture.</p></li>
</ul>
<h3 id="adjunctions-from-morphisms-3">Adjunctions from morphisms 3</h3>
<p>https://www.youtube.com/watch?v=tWhB7E-HS8Y</p>
<ul>
<li><p>Given $latex f : X \to Y$, define the “pullback” of a bundle over $latex Y$ to a bundle over $latex X$ written $latex f^* : \mathbf{Set}^Y \to \mathbf{Set}^X$: to each $latex x \in X$ we associate the fiber of $latex f(x) \in Y$. That is,</p>
<p>$latex f^* \hat{F} = \hat{F} \circ f.$</p></li>
<li><p>Now for the other direction. Idea: given a bundle over $latex X$ and $latex f :  X \to Y$, for each $latex y \in Y$ we have a set $latex f^{-1}(y) \subseteq X$ which are sent to that $latex y$ by $latex f$; we have to somehow combine their fibers to yield a fiber for $latex y$. Several ways we could imagine doing it: disjoint union, product? In this case we want the product. That is,</p>
<p>$latex \displaystyle (f_* \hat{E})(y) = \prod_{x \in f^{-1}(y)} \hat{E}(x).$</p></li>
</ul>
<p>(Foreshadowing: taking the disjoint union gives us another adjoint.)</p>
<h3 id="adjunctions-from-morphisms-4">Adjunctions from morphisms 4</h3>
<p>http://youtu.be/Cf7hCiTspJc</p>
<p>Proof of the adjunction $latex f^* \dashv f_*$. (Come up with your own mnemonic to remember which way around the adjunction goes; suggested: think of a “falling star”.)</p>
<ul>
<li><p>Notation: $latex \mathrm{Hom}_X(a,b)$ instead of $latex \mathbf{Set}^X(a,b)$ or $latex \mathrm{Hom}_{\mathbf{Set}^X}(a,b)$</p></li>
<li><p>Such hom-sets are a collection of maps between fibers, one for each base point.</p></li>
<li><p>So we have $latex \mathrm{Hom}_X(f^* \hat F, \hat E) = \prod_{x \in X}  \mathrm{Hom}(f^* \hat F (x), \hat E (x)) = \prod_{x \in X}  \mathrm{Hom}(\hat F (f(x)), \hat E (x))$.</p></li>
<li><p>We can partition $latex X$ as preimages of elements of $latex Y$ under $latex f$. So the above is equal to $latex \prod_{y \in Y} \prod_{x \in f^{-1}(y)}  \mathrm{Hom}(\hat F (y), \hat E (x))$.</p></li>
<li><p>A product of hom-sets is isomorphic to a hom-set into a product (<em>i.e.</em> $latex A^X B^X = (AB)^X$), so this is equal to $latex \prod_{y \in Y}  \mathrm{Hom}(\hat F (y), \prod_{x \in f^{-1}(y)} \hat E (x))$.</p></li>
<li><p>By definition of $latex f_*$, this is $latex \prod_{y \in Y}  \mathrm{Hom}(\hat F (y), (f_* \hat E) (y))$.</p></li>
<li><p>Finally, by definition of $latex \mathrm{Hom}_Y$, this is $latex \mathrm{Hom}_Y (\hat F, f_* \hat E)$.</p></li>
<li><p>Of course technically we would need to show naturality as well, but this is the basic idea.</p></li>
</ul>
<h3 id="adjunctions-from-morphisms-5">Adjunctions from morphisms 5</h3>
<p>https://www.youtube.com/watch?v=MSOGEtW39qM</p>
<p>Last time, we proved an adjunction $latex f^* \dashv f_*$, <em>i.e.</em></p>
<p>$latex \mathrm{Hom}_X (f^* \hat F, \hat E) \cong \mathrm{Hom}_Y (\hat F, f_* \hat E).$</p>
<p>In fact, we showed that both are isomorphic to</p>
<p>$latex \prod_{x \in X} \mathrm{Hom}(\hat F (f(x)), \hat E(x)).$</p>
<p><em>i.e.</em> given some $latex f : X \to Y$, for each $latex x$ we get a map going the <em>other</em> way, from the fiber over $latex f(x)$ to the fiber over $latex x$. (See the video for a nice picture.) But we can imagine turning these maps around, giving</p>
<p>$latex \prod_{x \in X} \mathrm{Hom}(\hat E(x), \hat F(f(x))).$</p>
<p>Using the same trick as last time, this is equivalent to $latex \prod_{y \in Y} \prod_{x \in f^{-1}(y)} \mathrm{Hom} (\hat E (x), \hat F (y))$, which is in turn equivalent to</p>
<p>$latex \prod_{y \in Y} \mathrm{Hom} (\coprod_{x \in f^{-1}(y)} \hat E (x), \hat F (y))$</p>
<p>(since $latex \mathrm{Hom}(-,B)$ turns limits into colimits; concretely, note that $latex A^X A^Y = A^{X + Y}$).</p>
<p>This gives us a left adjoint $latex f_! \dashv f^*$, defined by</p>
<p>$latex \displaystyle (f_! \hat E)(y) = \coprod_{x \in f^{-1}(y)} \hat E(x).$</p>
<p>Remark: note that if we view bundles over $latex X$ as objects of the slice category $latex \mathbf{Set}/X$, $latex f_!$ is just composition.</p>
<h2 id="double-categories">Double Categories</h2>
<h3 id="double-categories-1">Double Categories</h3>
<p>https://www.youtube.com/watch?v=kiCZiSA2W3Q</p>
<p>Internal categories in $latex \mathbf{Cat}$. Recall that an internal category in $latex E$ is a pair of objects $latex A$ (representing objects) and $latex B$ (representing morphisms), and a pair of parallel arrows $latex s,t : B \to A$ in $latex E$ recording the source and target of each morphism, all suitably equipped with unit and composition.</p>
<p>If $latex A$ and $latex B$ are themselves categories, and $latex s$ and $latex t$ are functors, then $latex B$ itself has sets of objects $latex B_0$ and morphisms $latex B_1$ with source and target functions, and the same for $latex A$. Then the functors $latex s$ and $latex t$ have actions on morphisms and objects, so we get a square with two parallel arrows on each side.</p>
<ul>
<li>$latex A_0$ are $latex 0$-cells.</li>
<li>$latex A_1$ are “vertical $latex 1$-cells”.</li>
<li>$latex B_0$ are “horizontal $latex 1$-cells”.</li>
<li>$latex B_1$ are $latex 2$-cells, which sit inside <em>squares</em> (not inside the area between two parallel $latex 1$-cells): each element of $latex B_1$ has corresponding sources and targets in both $latex B_0$ and $latex A_1$, and the double commuting square described above ensures that the sources and targets of <em>those</em> have to match up in a square.</li>
</ul>
<p>What about composition? Note $latex B$ and $latex A$ already come equipped with composition, which together give us “vertical composition” of $latex 2$-cells. Composition in the internal category gives horizontal composition of $latex 2$-cells.</p>
<p>Note if all vertical $latex 1$-cells are identities, this collapses to the usual idea of a $latex 2$-category. (Or symmetrically, with horizontal $latex 1$-cells being identities.)</p>
<h2 id="spans">Spans</h2>
<h3 id="spans-1">Spans 1</h3>
<p>https://www.youtube.com/watch?v=SQfUXOCMUhI</p>
<p>NOTE: There seems to be no catsters video actually explaining what a “bicategory” is. <a href="https://ncatlab.org/nlab/show/bicategory">According to the nlab</a> it is a weaker version of a 2-category, where certain things are required to hold only up to coherent isomorphism rather than on the nose.</p>
<p>Let $latex E$ be a category with (chosen) pullbacks. $latex \mathbf{Span}(E)$ is a bicategory with</p>
<ul>
<li>$latex 0$-cells the objects of $latex E$</li>
<li>$latex 1$-cells $latex A \to B$ are spans $latex A \leftarrow S \rightarrow B$.</li>
<li><p>$latex 2$-cells are morphisms between $latex 1$-cells, that is, spans. So a $latex 2$-cell between $latex A \leftarrow S \rightarrow B$ and $latex A \leftarrow S'  \rightarrow B$ is a morphism $latex S \to S'$ which makes things commute.</p></li>
<li><p>$latex 1$-cell composition: use pullback. Is this associative? Yes, up to isomorphism (because of universal property of pullbacks) but not on the nose. (Hence we get a bicategory and not a strict $latex 2$-category.)</p></li>
<li>Vertical $latex 2$-cell composition: just composition of the underlying morphisms.</li>
<li><p>Horizontal $latex 2$-cell composition: the two pullbacks induce a morphism between them.</p></li>
</ul>
<p>Can check all the axioms etc.</p>
<p>Now, note monads can be constructed inside any bicategory, and are given by</p>
<ul>
<li>a $latex 0$-cell $latex X$</li>
<li>a $latex 1$-cell $latex T : X \to X$</li>
<li>$latex 2$-cells $latex \eta$ and $latex \mu$ satisfying the usual monad axioms (slightly modified to make them make sense)</li>
</ul>
<p>It turns out that monads in $latex \mathbf{Span}(E)$ are great! For example, monads in $latex \mathbf{Span}(\mathbf{Set})$ are small categories. Next time we’ll see why.</p>
<h3 id="spans-2">Spans 2</h3>
<p>https://www.youtube.com/watch?v=Jn5dZuebeXU</p>
<p>Monads in $latex \mathbf{Span}(Set)$ are small categories. These notes make a lot more sense when you can look at the diagrams. Watch the video or work out the diagrams yourself.</p>
<p>We have</p>
<ul>
<li>a $latex 0$-cell, <em>i.e.</em> a set $latex C_0$.</li>
<li>a $latex 1$-cell from $latex C_0$ to itself, <em>i.e.</em> a span $latex C_0  \stackrel{s}{\leftarrow} C_1 \stackrel{t}{\rightarrow} C_0$ (idea is that $latex C_1$ will be the set of morphisms, and $latex s$ and $latex t$ will pick out the source and target objects)</li>
<li>a $latex 2$-cell $latex \eta$ from $latex 1$ (the boring span with all the same object and identity morphisms) to the $latex 1$-cell given above. This ends up being a function $latex f : C_0 \to C_1$ such that $latex f ; s = f ; t = id$, that is, $latex f$ takes each object in $latex C_0$ to a morphism in $latex C_1$ having that object as both its source and target.</li>
<li>a $latex 2$-cell $latex \mu : C_1^2 \to C_1$. $latex C_1^2$ is given by a pullback: a pair of morphisms such that the target of the first equals the source of the second, <em>i.e.</em> a composable pair. $latex \mu$ therefore has to take a composable pair and produce a single morphism in $latex C_1$ such that its source equals the source of the first and its target equals the target of the second.</li>
</ul>
<p>And of course there are some monad laws which amount to the category laws.</p>
<p>More generally, monads in $latex \mathbf{Span}(E)$ are categories internal to $latex E$. <em>I.e.</em></p>
<ul>
<li>an “objects object” $latex C_0$</li>
<li>a “morphisms object” $latex C_1$</li>
<li>source and target maps $latex C_1 \to C_0$</li>
<li>identities and composition as before.</li>
</ul>
<h2 id="multicategories">Multicategories</h2>
<h3 id="multicategories-1">Multicategories 1</h3>
<p>http://www.youtube.com/watch?v=D_pPNgGZYDs</p>
<p>Like categories, but morphisms have multiple objects as their source.</p>
<p>A (small) multicategory $latex C$ is given by</p>
<ul>
<li>a set of objects, $latex \mathrm{ob} C$</li>
<li>For all $latex x_1, \dots, x_n, x$, a set $latex C(x_1, \dots, x_n, x)$ of morphisms.</li>
<li>Composition: a morphism with $latex n$ inputs can be composed with $latex n$ morphisms, producing a result which is a morphism with the concatenation of inputs of all the $latex n$ morphisms.</li>
<li>Identity morphisms, with just one input and output.</li>
</ul>
<p>Note that one can have a morphism with <em>no</em> inputs.</p>
<p>This can all be expressed nicely using the “free monoid monad” (i.e. list monad). Let $latex T$ be the free monoid monad on $latex \mathbf{Set}$, i.e. the list monad; that is, $latex T$ sends each set $latex S$ to the free monoid on $latex S$ (i.e. lists of $latex S$).</p>
<p>Make a bicategory of $latex T$-spans. Just as monads in the category of spans were small categories, monads in the (bi)category of $latex T$-spans are multicategories.</p>
<p>$latex T$-span has:</p>
<ul>
<li>$latex 0$-cells are sets</li>
<li>$latex 1$-cells are spans $latex T A \leftarrow S \to B$.</li>
<li>$latex 2$-cells are just span morphisms.</li>
<li>Composition uses pullbacks and multiplication of $latex T$.</li>
<li>Identity is $latex T$-span $latex T A \leftarrow A \to A$ using $latex \eta$ and $latex id$.</li>
</ul>
<p>Bicategory axioms follow from monad laws for $latex T$. Next time: monads in this category are multicategories.</p>
<h3 id="multicategories-2">Multicategories 2</h3>
<p>https://www.youtube.com/watch?v=WytjdlserwU</p>
<p>We’ve seen that monads in Span are categories.</p>
<p>We’ve seen a category of $latex T$-spans, spans with a $latex T$ on the left. We’ll see that monads in $latex T$-$latex \mathbf{Span}$ are multicategories.</p>
<p>Recall that $latex T$ is the list monad.</p>
<p>A monad in $latex T$-$latex \mathbf{Span}$ is:</p>
<ul>
<li>a set $latex C_0$ (which will represent objects of the multicategory)</li>
<li>a $latex 1$-cell $latex C_0 \to C_0$ is a $latex T$-span, i.e. an object $latex C_1$ (representing morphisms of the multicategory) together with morphisms from $latex C_1$ to $latex T\ C_0$ (picking out the sequence of input objects) and from $latex C_1$ to $latex C_0$ (picking the target object).</li>
<li>a $latex 2$-cell $latex \eta$, representing the identity morphism with a single input (see video for a commutative diagrma)</li>
<li>a $latex 2$-cell $latex \mu$ which represents composition in a multicategory. See video for diagram!</li>
</ul>
<p>Key point: we can actually do this with other monads $latex T$! And even on other categories with pullbacks, as long as $latex T$ preserves pullbacks (and $latex \eta$ and $latex \mu$ commutative diagrams are pullbacks). This yields a notion of a $latex T$-multicategory. The source of each morphism is not just a list of objects but a $latex T$-structure of objects.</p>
<h2 id="metric-spaces-and-enriched-categories">Metric Spaces and Enriched Categories</h2>
<h3 id="metric-spaces-and-enriched-categories-1">Metric Spaces and Enriched Categories 1</h3>
<p>https://www.youtube.com/watch?v=be7rx29eMr4</p>
<p>Idea due to Lawvere. A metric $latex d$ on a metric space satisfies:</p>
<ul>
<li>Triangle inequality: $latex d(a,b) + d(b,c) \geq d(a,c)$</li>
<li>$latex 0 \geq d(a,a)$</li>
</ul>
<p>Compare to the data for a category, written in a slightly funny way:</p>
<ul>
<li>$latex \mathrm{Hom}(a,b) \times \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$</li>
<li>$latex \{\star\} \to \mathrm{Hom}(a,a)$</li>
</ul>
<p>These look remarkably similar! In fact, they are both examples of enriched category. We’ll start with a normal category and show how to generalize it to an enriched category.</p>
<p>Let $latex C$ be a category. We have:</p>
<ul>
<li>a collection $latex \mathrm{Ob}(C)$</li>
<li>$latex \forall a,b \in \mathrm{Ob}(C)$, $latex \mathrm{Hom}(a,b) \in \mathrm{Ob}(\mathbf{Set})$</li>
<li>$latex \forall a,b,c \in \mathrm{Ob}(C)$, $latex \exists \mathrm{Hom}(a,b) \times \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$</li>
<li>$latex \forall a \in \mathrm{Ob}(C)$, $latex \exists \{\star\} \to \mathrm{Hom}(a,a)$</li>
<li>…satisfying associativity and unit laws.</li>
</ul>
<p>Important thing to note: composition and identity are morphisms in $latex \mathbf{Set}$. What properties of $latex \mathbf{Set}$ have we used? Just a Cartesian product and the one-element set $latex \{\star\}$. Right generalization is a monoidal category.</p>
<p>In particular, if $latex (V, \otimes, I)$ is a monoidal category, we can define <em>categories enriched in $latex V$</em>. Definition becomes:</p>
<p>$latex C$ is a $latex V$-category (category enriched in $latex V$):</p>
<ul>
<li>collection $latex \mathrm{Ob}(C)$ as before</li>
<li>$latex \mathrm{Hom}(a,b) \in \mathrm{Ob}(V)$, <em>i.e</em> we don’t have hom-<em>sets</em> but hom-<em>objects</em> that live in the category $latex V$</li>
<li>The composition map is a morphism $latex \mathrm{Hom}(a,b) \otimes \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$ in $latex V$</li>
<li>Identity morphisms are now given by a morphism $latex I \to \mathrm{Hom}(a,a)$ in $latex V$.</li>
<li>…satisfying associativity and unit laws.</li>
</ul>
<p><em>e.g.</em> pick $latex V$ to be category of Abelian groups (yields “additive category”), or category of vector spaces (yields “linear categories”).</p>
<p>What if we take $latex V$ to be a non-concrete category? <em>e.g.</em> take the poset of nonnegative real numbers under $latex \geq$. Can make this monoidal by taking the usual $latex +$, identity is $latex 0$. Then it turns out that categories enriched in this poset category are metric spaces!</p>
<h3 id="metric-spaces-and-enriched-categories-2">Metric Spaces and Enriched Categories 2</h3>
<p>https://www.youtube.com/watch?v=0p3iS3Nf-fs</p>
<p>Explains in more detail how categories enriched in $latex (\mathbb{R}^+, \geq)$ poset (with monoidal structure given by $latex +$ and $latex 0$) are metric spaces.</p>
<ul>
<li>For each pair of objects we have a “Hom-object” which is a nonnegative real number. (“distance”)</li>
<li>Composition is supposed to be a morphism $latex \mathrm{Hom}(a,b) \otimes  \mathrm{Hom}(b,c) \to \mathrm{Hom}(a,c)$. But $latex \otimes$ is $latex +$ and $latex \to$ is $latex \geq$, so this is the triangle inequality.</li>
<li>Identity morphisms are given by $latex I \to \mathrm{Hom}(a,a)$; in this context that says $latex 0 \geq \mathrm{Hom}(a,a)$, i.e. distance from $latex a$ to itself is $latex 0$.</li>
<li>Associativity and unit laws are vacuous.</li>
</ul>
<p>This is actually a <em>generalized</em> metric space. More general than a metric space in several ways:</p>
<ul>
<li>Distance is not symmetric: $latex d(a,b) \neq d(b,a)$. This actually has many real-world models (e.g. time to go between towns in the Alps).</li>
<li>We might have $latex d(a,b) = 0$ for $latex a \neq b$.</li>
<li>We allow “infinite” distances (???)</li>
</ul>
<p>Now we can study metric spaces categorically.</p>
<p>Given two $latex V$-categories, a $latex V$-functor $latex \Phi : C \to D$ consists of</p>
<ul>
<li>a map $latex \mathrm{Ob}(C) \to \mathrm{Ob}(D)$, and</li>
<li>for all $latex a,b,c \in C$, $latex \Phi : \mathrm{Hom}(a,b) \to  \mathrm{Hom(\Phi a, \Phi b)}$ (a morphism in $latex D$) which has to commute with composition in the appropriate way.</li>
</ul>
<p>In the generalized metric setting, such a $latex \Phi$ is a nonexpansive map.</p>
<h3 id="metric-spaces-and-enriched-categories-3">Metric Spaces and Enriched Categories 3</h3>
<p>https://www.youtube.com/watch?v=kMSt_Ci54BE</p>
<p>Enriched natural transformations. There are actually various ways to generalize. Today: simple-minded version. Apparently if we generalize the notion of a <em>set</em> of natural transformations we get a slightly better definition—this will be covered in future videos. [editor’s note: to my knowledge no such future videos exist.]</p>
<p>Standard definition of a natural transformation $latex \theta : F \to G$ given functors $latex F,G : C \to D$. Problem: we are supposed to have $latex \theta_x \in \mathrm{Hom}(F x, G x)$, but in the enriched setting $latex \mathrm{Hom}(F x, G x)$ may not be a <em>set</em>, but just some object. So what should it mean for $latex \theta_x$ to be an “element” of it?</p>
<p>Simple way around this: let $latex (V, \otimes, 1)$ be a monoidal category. We can define a canonical functor $latex \Gamma : V \to \mathrm{Set}$ (“generalized elements”) which takes an object $latex v \in V$ to the Hom-set $latex \mathrm{Hom}(1,v)$.</p>
<p>e.g. if $latex V = \mathrm{Set}$, $latex \Gamma$ is the identity. Another example I don’t understand involving category of complex vector spaces.</p>
<p>In the example we care about, $latex V = \mathbb{R}^+$, and $latex I = 0$. In this case $latex \Gamma$ sends $latex v$ to the Hom-set $latex 0 \geq v$, that is, $latex \{\star\}$ if $latex v = 0$ and the empty set otherwise.</p>
<p>So now we can say that $latex \theta_x$ should be a <em>generalized element</em> of $latex \mathrm{Hom}(F x, G x)$.</p>
<p>So, let $latex F, G$ be $latex V$-functors. Define a $latex V$-natural transformation $latex \theta : F \to G$ as</p>
<ul>
<li>for all $latex x \in C$, $latex \theta_x \in \Gamma(\mathrm{Hom}(F x, G x))$.</li>
<li>Big commuting diagram expressing naturality in the enriched setting (see the video).</li>
</ul>

