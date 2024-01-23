---
title: 'New 2D text layout library'
published: 2009-08-14T16:19:28Z
categories: haskell,projects
tags: boxes,layout,pretty printing,text
---

I've just uploaded to Hackage a small library, <a href="http://hackage.haskell.org/package/boxes">boxes</a>, for 2D text layout/pretty-printing.  This is something I've wanted a couple times and have heard others ask for---<a href="http://hackage.haskell.org/package/pretty">Text.PrettyPrint.HughesPJ</a>, as nice as it is, is intended for pretty-printing only <i>linear</i> output (such as source code), and is ill-suited for layout of a two-dimensional nature, such as columns of text, proof trees, and the like.

I wrote this library almost a year ago and have been sitting on it ever since, and finally decided that I really ought to just release what I have so others can make use of it.  It's quite usable as it is, and has (I hope) solid documentation.  There's nothing in the way of examples or tutorials or QuickCheck properties, and there are many more features one could imagine adding to it, but it's simply not a priority for me right now, so it is what it is.  However, if anyone is interested in taking over as maintainer and extending it, be my guest!  Just fork off a new <a href="http://code.haskell.org/~byorgey/code/boxes/">darcs repo</a> and hack away.

