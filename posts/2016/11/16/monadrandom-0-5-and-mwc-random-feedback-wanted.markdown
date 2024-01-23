---
title: 'MonadRandom 0.5 and mwc-random: feedback wanted'
published: 2016-11-16T11:36:03Z
categories: haskell,projects
tags: monad,random
---

<p>Since 2013 or so I have been the maintainer of the <a href="http://hackage.haskell.org/package/MonadRandom"><code>MonadRandom</code></a> package, which provides an <code>mtl</code>-style type class for monads with support for generation of pseudorandom values, along with a concrete random monad transformer <code>RandT</code>. As of this writing it has <a href="http://packdeps.haskellers.com/reverse/MonadRandom">89 reverse dependencies on Hackage</a>—a healthy number, and one that makes me think carefully about any breaking changes to the package.</p>
<p>Recently I got a number of pull requests, and have been working on putting together an 0.5 release which adds a few functions, adds lazy- and strict-state variants of <code>RandT</code>, and reorganizes things to be closer to standard practice of the <code>transformers</code> package. Since this release will include some technically breaking changes already, it’s a good time to think about potentially including others.</p>
<p>The one thing I am not sure what to do about is this issue: <a href="https://github.com/byorgey/MonadRandom/issues/26">Allow MonadRandom interface for MWC-random</a>. <code>mwc-random</code> is a very nice package for psuedorandom number generation, but apparently it does not fit into the <code>MonadRandom</code> abstraction. First of all, I would like to understand why—I am not very familiar with <code>mwc-random</code>. Second of all, I’d love to figure out a solution, but ideally one that causes as little breakage to existing code as possible.</p>
<p>Leave a comment (either here or on the <a href="https://github.com/byorgey/MonadRandom/issues/26">github issue</a>) if this is something you know/care about, and let’s see if we can figure out a good solution together!</p>

