---
title: Mapping over a nested functor?
published: 2007-08-16T13:49:43Z
categories: haskell,learning
tags: 
---

The other day I had a triply-nested list of type [[[a]]], and found myself applying this function to it:
<pre>
sort . map sort . map (map sort)</pre>
This annoyed me.  I want a way to say, "map this function over every level of this nested functor".  In other words, given a function <tt>transf :: (Functor f) =&gt; f a -&gt; f a</tt>, I want a function <tt>nfmap</tt> which, given <tt>transf</tt> and a "nested functor" (i.e. something of type f a, or f (f a), or f (f (f a)), or ...) applies <tt>transf</tt> on all levels (i.e. <tt>transf . fmap transf . fmap (fmap transf) ...</tt>). Ideally, then, I would be able to write <tt>nfmap sort :: [[[a]]] -&gt; [[[a]]]</tt> in place of the code that annoyed me.

Sadly, how to implement the type class <tt>NestedFunctor</tt> eludes me at the moment.  Has anything like this been done before? (I'd be surprised if it hasn't.)  Anyone with some serious Haskell-type-fu want to show me how to implement this?

