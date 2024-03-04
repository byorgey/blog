---
katex: true
title: "Improving GHC's constraint solving"
published: 2010-06-17T17:25:18Z
categories: haskell
tags: constraints,GHC,inference,MSR,type
---

So I've been here at <a href="http://research.microsoft.com/en-us/labs/cambridge/">MSR Cambridge</a> for almost two weeks now (!), working in the <a href="http://research.microsoft.com/en-us/groups/ppt/default.aspx">Programming Principles and Tools Group</a> with <a href="http://research.microsoft.com/en-us/people/simonpj/">Simon Peyton-Jones</a> and <a href="http://research.microsoft.com/en-us/people/dimitris/">Dimitrios Vytiniotis</a> --- and also collaborating with <a href="http://www.cis.upenn.edu/~sweirich/">Stephanie Weirich</a> and <a href="http://www.cis.upenn.edu/~stevez/">Steve Zdancewic</a>, who are also in Cambridge.  So, what have I been doing?

This week, Simon, Dimitris, and I have been having a major GHC hacking session, implementing the new constraint-solving type inference engine described in their <a href="http://haskell.org/haskellwiki/Simonpj/Talk:OutsideIn">new OutsideIn(X) paper</a>.  It's been a lot of fun --- I've never hacked on GHC before, and it's quite a new experience hacking on such a large (and amazing) piece of software.  I've been working on the <i>constraint canonicaliser</i>, which puts constraints into canonical forms appropriate for the constraint simplifier to work with.  As a simple example, the equality constraint <code>(Int, a) ~ (b, Char)</code> gets decomposed into the primitive constraints <code>Int ~ b</code> and <code>a ~ Char</code>.  It's also responsible for flipping and flattening equality constraints so that type function applications only happen on the left-hand side: for example, the constraint <code>F a b ~ G Int</code> (where both <code>F</code> and <code>G</code> are type families), gets rewritten to a pair of constraints <code>F a b ~ c</code>, <code>G Int ~ c</code>, where <code>c</code> is a fresh variable.  If we didn't do this it would lead to problems with termination: for example, the constraint <code>a ~ [F a]</code> could lead to infinite rewriting of <code>a</code> to <code>[F a]</code> to <code>[F [F a]]</code> to... (And before you protest that we ought to just throw out <code>a ~ [F a]</code> on the grounds that it is recursive, note that <code>F a</code> may not mention <code>a</code> at all; for example, perhaps <code>F</code> is defined by <code>F [x] = Int</code>.)

Constraints also include type class constraints and implicit parameter constraints, although there's much less work to do with those as far as canonicalisation is concerned.

Next week, I'll likely get back to our other, more ambitious research project, but I'll write more about that next time.  In the meantime, if you're in or near Cambridge and want to meet up, just drop me a note!

