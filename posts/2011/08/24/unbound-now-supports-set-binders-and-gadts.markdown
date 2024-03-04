---
katex: true
title: 'Unbound now supports "set" binders and GADTs'
published: 2011-08-24T20:32:16Z
categories: haskell,projects
tags: 
---

I have just <a href="http://hackage.haskell.org/package/unbound">uploaded version 0.3</a> of the <a href="http://byorgey.wordpress.com/2011/03/28/binders-unbound/" title="Binders Unbound">Unbound library</a> (and <a href="http://hackage.haskell.org/package/RepLib">version 0.5 of RepLib</a>).  This version adds support for two major new features:

<ul>
	<li>The new <code>permbind</code> and <code>setbind</code> functions create binders that are alpha-equivalent "up to permutation".  For example, when quantifying over a list of type variables, their order really does not matter: $\forall a b. a \to b \approx \forall b a. a \to b$.  Unbound now lets you express this and takes it into account when testing alpha-equivalence.</li>
	<li>RepLib (and hence Unbound) now supports GADTs, as long as they do not contain any existential quantification.</li>
</ul>

Enjoy!

