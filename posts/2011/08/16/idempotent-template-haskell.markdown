---
title: Idempotent Template Haskell
published: 2011-08-16T15:09:20Z
categories: haskell
tags: conditional,generation,template
---

Yesterday I was looking for a way to have some Template Haskell code generate a definition for a certain identifier if and only if such a definition did not already exist.  Essentially I want to be able to call the code multiple times but have it generate something only once.  This turns out to be extremely easy but non-obvious, so I thought I'd write it here for posterity.  The trick is that <code>reify</code> throws an error if called on an identifier that doesn't exist, and <code>recover</code> can be used to catch TH errors. Thus:

<pre><code>
recover (generateBinding n) (reify n &gt;&gt; return [])
</code></pre>

which uses <code>generateBinding n</code> to "recover" from the error of <code>n</code>'s non-existence.

