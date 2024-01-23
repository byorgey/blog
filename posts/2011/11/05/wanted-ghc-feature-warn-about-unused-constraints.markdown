---
title: Wanted GHC feature: warn about unused constraints
published: 2011-11-06T01:03:52Z
categories: haskell
tags: constraints,GHC,unused,warning
---

Consider the following function:

<code><pre>
foo :: (Show a, Ord a) =&gt; a -&gt; a -&gt; String
foo x y = show x ++ show y
</pre></code>

Currently (as of GHC 7.2.1) when compiling this code with <code>-Wall</code>, no warnings are generated.  But I'd really love to get a warning like

<code><pre>
Foo.hs:1:1: Warning: Unused constraint: Ord a
</pre></code>

I see no theoretical impediments here.  At the level of fully desugared and elaborated GHC core, it is no harder to tell which constraints are unused than which arguments are unused, because constraints <i>are</i> arguments.

One possible objection is that there is some inherent ambiguity here.  For example, consider:

<code><pre>
bar :: (Eq a, Ord a) =&gt; a -&gt; a -&gt; String
bar x y | x == y    = "yay"
        | otherwise = "boo"
</pre></code>

When elaborating <code>bar</code>, GHC has a free choice of where to get the needed <code>(==)</code> method: from the <code>Eq</code> constraint or from the <code>Eq</code> superclass of the <code>Ord</code> constraint.  So we might get a warning about <code>Eq</code> being redundant or about <code>Ord</code> being redundant, depending on which one it decides to use.  But I don't see this as a big problem.

I think this could make for a nice project for someone wanting to dig into hacking on GHC.  Perhaps I'll do it myself at some point if no one else takes it on.

