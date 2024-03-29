---
katex: true
title: 'Types versus sets in math and programming languages'
published: 2021-06-24T11:34:50Z
categories: projects,teaching
tags: foundations,language,math,pedagogy,programming,sets,types
---

<p>For several years I have been designing and implementing a <a href="https://github.com/disco-lang/disco/">functional teaching language especially for use in the context of a Discrete Mathematics course</a>. The idea is for students to be exposed to some functional and statically-typed programming early in their computer science education, and to give them a fun and concrete way to see the connections between the concepts they learn in a Discrete Math course and computation. I am <a href="https://cs.wheaton.edu/~tvandrun/dmfp/">not the first to think of combining FP + Discrete Math</a>, but I think there is an opportunity to do it really well with a language designed expressly for the purpose. (And, who am I kidding, designing and implementing a language is just plain fun.)</p>
<p>Of course the language has an expressive static type system, with base types like natural numbers, rationals, Booleans, and Unicode characters, as well as sum and product types, lists, strings, and the ability to define arbitrary recursive types. It also has built-in types and syntax for finite sets. For example,</p>
<pre><code>A : Set ℕ
A = {1, 3, 6}</code></pre>
<p>(Incidentally, I will be using Unicode syntax since it looks nice, but there are also ASCII equivalents for everything.) Sets support the usual operations like union, intersection, and difference, as well as set comprehension notation. The intention is that this will provide a rich playground for students to play around with the basic set theory that is typically taught in a discrete math class.</p>
<h2 id="but-wait">But wait…</h2>
<p>Hopefully the above all seems pretty normal if you are used to programming in a statically typed language. Unfortunately, there is something here that I suspect is going to be deeply confusing to students. I am so used to it that it took me a long time to realize what was wrong; maybe you have not realized it either. (Well, perhaps I gave it away with the title of the blog post…)</p>
<p>In a math class, we typically tell students that $\mathbb{N}$ is a <em>set</em>. But in Disco, <code>ℕ</code> is a <em>type</em> and something like <code>{1,2,3}</code> is a set. If you have been told that $\mathbb{N}$ is a set, the distinction is going to seem very weird and artificial to you. For example, right now in Disco, you can ask whether <code>{1,2}</code> is a subset of <code>{1,2,3}</code>:</p>
<pre><code>Disco&gt; {1,2} ⊆ {1,2,3}
true</code></pre>
<p>But if you try to ask whether <code>{1,2}</code> is a subset of <code>ℕ</code>, you get a syntax error:</p>
<pre><code>Disco&gt; {1,2} ⊆ ℕ
1:10:
  |
1 | {1,2} ⊆ ℕ
  |          ^
keyword "ℕ" cannot be used as an identifier</code></pre>
<p>Now, we could try various things to improve this particular example—at the very least, make it fail more gracefully. But the fundamental question remains: what is the distinction between types and sets, and why is it important? If it’s not important, we should get rid of it; if it is important, then I need to be able to explain it to students!</p>
<p>We could try to completely get rid of the distinction, but this seems like it would lead directly to a dependent type system and refinement types. Refinement types <a href="https://ucsd-progsys.github.io/liquidhaskell-blog/">are super cool</a> but I really don’t think I want to go there (Disco’s type system is <a href="https://github.com/disco-lang/disco/issues/207">already complicated enough</a>).</p>
<p>However, I think there actually is an important distinction; this blog post is my first attempt at crystallizing my thoughts on the distinction and how I plan to explain it to students.</p>
<h2 id="types-vs-sets">Types vs sets</h2>
<p>So what is the difference between sets and types? The slogan is that types are <em>intensional</em>, whereas sets are <em>extensional</em>. (I won’t actually use those words with my students.) That is:</p>
<ul>
<li>Sets are characterized by the $\in$ relation: we can <em>ask</em> which items are elements of a set and which are not.</li>
<li>Types, on the other hand, are characterized by how elements of the type are built: we can <em>construct</em> elements of a type (and <em>deconstruct</em> them) in certain ways specific to the type.</li>
</ul>
<p>This seems kind of symmetric, but it is not. You can’t ask whether a thing is an element of a set if you don’t know how to even make or talk about any things in the first place. So types are prior to sets: types provide a universe of values, constructed in orderly ways, that we can work with; only then can we start picking out certain values to place them in a set.</p>
<p>Of course, this all presupposes some kind of type theory as foundational. Of course I am aware that one can instead take axiomatic set theory as a foundation and build everything up from the empty set. But I’m building a typed functional programming language, so of course I’m taking type theory as foundational! More importantly, however, it’s what almost every working mathematician does <em>in practice</em>. No one actually works or thinks in terms of axiomatic set theory (besides set theorists). Even in a typical math class, some sets are special. Before we can talk about the set ${1,3,6}$, we have to introduce the special set $\mathbb{N}$ so we know what $1$, $3$, and $6$ are. Before we can talk about the set ${(1,1), (3,5), (6,8)}$ we have to introduce the special Cartesian product operation on sets so we know what tuples are. And so on. We can think of types as a <em>language</em> for describing this prior class of special sets.</p>
<h2 id="explaining-things-to-students">Explaining things to students</h2>
<p>So what will I actually say to my students? First of all, when introducing the language, I will tell them about various built-in primitive types like naturals, rationals, booleans, and characters. I won’t make a big deal about it, and I don’t think I will need to: for the most part they will have already seen a language like Python or Java with types for primitive values.</p>
<p>When we get to talking about sets, however (usually the second unit, after starting with propositional logic), we will define sets as collections of values, and I will explicitly point out the similarity to types. I will tell them that types are <em>special built-in sets</em> with rules for building their elements. We will go on to talk about disjoint union and Cartesian product, and practice building elements of sum and product types. (When we later get to recursion, they will therefore have the tools they need to start building recursive types such as lists and trees.)</p>
<p>The other thing to mention will be the way that when we write the type of a set, as in, <code>Set ℕ</code>, we have to write down the type of the elements—in other words, the <em>universe</em>, or ambient set from which the elements are chosen. When introducing set theory, traditionally one mentions universe sets only when talking about the set complement operation; but the fact is that mathematicians always have some universe set in mind when describing a given set.</p>
<p>Now, coming back to the example of <code>{1,2} ⊆ ℕ</code>, it would still be confusing for students if this is a syntax error, and I have some ideas about how to make it work. Briefly, the idea is to allow types to be used in expressions (but not the other way around!), with <code>T : Set T</code>. If I tell them that types are special sets, then logically they will expect to be able to use them as such! However, this is an extremely nontrivial change: it means that Disco would now be able to represent <em>infinite</em> sets, requiring sets to be internally represented via a deep embedding, rather than simply storing their elements (as is currently the case). For example, <code>2 ∈ (ℕ \ {3,5})</code> should evaluate to <code>true</code>, but we obviously can’t just enumerate all the elements of <code>ℕ \ {3,5}</code> since there are infinitely many. More on this in a future post, perhaps!</p>

