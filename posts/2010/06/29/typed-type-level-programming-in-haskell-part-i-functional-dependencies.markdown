---
title: Typed type-level programming in Haskell, part I: functional dependencies
published: 2010-06-29T11:29:55Z
categories: haskell
tags: addition,functional dependencies,programming,type-level
---

The other project I'm working on at MSR this summer is a bit more ambitious: our headline goal is to extend <a href="http://haskell.org/ghc">GHC</a> to enable <i>typed, functional, type-level programming</i>.  What's that, you ask?  Well, first, let me tell you a little story...

Once upon a time there was a lazy*, pure, functional programming language called <a href="http://haskell.org">Haskell</a>.  It was very careful to always keep its values and types strictly separated.  So of course "type-level programming" was completely out of the question!  ...or was it?

In 1997, along came <i>multi-parameter type classes</i>, soon followed by <a href="http://haskell.org/haskellwiki/Functional_dependencies">functional dependencies</a>.  Suddenly, type-level programming became possible (and even fun and profitable, depending on your <a href="http://okmij.org/ftp/Haskell/types.html">point of view</a>).  How did this work?

Whereas normal type classes represent <i>predicates</i> on types (each type is either an instance of a type class or it isn't), multi-parameter type classes represent <i>relations</i> on types.  For example, if we create some types to represent natural numbers,
<pre>
  data Z
  data S n
</pre>
we can define a multi-parameter type class <code>Plus</code> which encodes the addition relation on natural numbers:
<pre>
  class Plus m n r

  instance Plus Z n n
  instance (Plus m n r) =&gt; Plus (S m) n (S r)
</pre>

This says that for any types <code>m</code>, <code>n</code>, and <code>r</code>, <code>(Z,n,n)</code> are in the <code>Plus</code> relation, and <code>(S m, n, S r)</code> are in the <code>Plus</code> relation whenever <code>(m,n,r)</code> are.  We can load this into <code>ghci</code> (after enabling a few extensions, namely <code>MultiParamTypeClasses</code>, <code>FlexibleInstances</code>, and <code>EmptyDataDecls</code>), but unfortunately we can't yet actually use the <code>Plus</code> relation to do any type-level <i>computation</i>:

<pre>
  *Main&gt; :t undefined :: (Plus (S Z) (S Z) r) =&gt; r
  undefined :: (Plus (S Z) (S Z) r) =&gt; r :: (Plus (S Z) (S Z) r) =&gt; r
</pre>

We asked for the type of something which has type <code>r</code>, given that the relation <code>Plus (S Z) (S Z) r</code> holds -- but notice that ghci was not willing to simplify that constraint at all.  The reason is that type classes are <i>open</i> -- there could be lots of instances of the form <code>Plus (S Z) (S Z) r</code> for many different types <code>r</code>, and <code>ghci</code> has no idea which one we might want.

To the rescue come functional dependencies, which let us specify that some type class parameters are determined by others -- in other words, that the relation determined by a multi-parameter type class is actually a <i>function</i>.

<pre>
  class Plus m n r | m n -&gt; r
  instance Plus Z n n
  instance (Plus m n r) =&gt; Plus (S m) n (S r)
</pre>

Here we've added the functional dependency <code>m n -&gt; r</code>, which says that knowing <code>m</code> and <code>n</code> also determines <code>r</code>.  In practice, this means that we are only allowed to have a single instance of <code>Plus</code> for any particular combination of <code>m</code> and <code>n</code>, and if ghc can determine <code>m</code> and <code>n</code> and finds an instance matching them, it will assume it is the only one and pick <code>r</code> to match.  Now we can actually do some computation (after enabling <code>UndecidableInstances</code>):

<pre>
  *Main&gt; :t undefined :: (Plus (S Z) (S Z) r) =&gt; r
  undefined :: (Plus (S Z) (S Z) r) =&gt; r :: S (S Z)
</pre>

Aha!  So 1 + 1 = 2, at the type level!

Type-level programming with multi-parameter type classes and functional dependencies has a strong resemblance to <a href="http://en.wikipedia.org/wiki/Logic_programming">logic programming</a> in languages like Prolog.  We declare rules defining a number of relations, and then "running" a program consists of searching through the rules to find solutions for unconstrained variables in a given relation.  (The one major difference is that GHC's type class instance search doesn't (yet?) do any backtracking.)

This is getting a bit long so I'll break it up into a few posts.  In the next installment, I'll explain type families, which are a newer, alternative mechanism for type-level programming in Haskell.

* OK, OK, non-strict.

