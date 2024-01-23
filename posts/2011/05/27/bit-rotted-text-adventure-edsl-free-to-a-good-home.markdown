---
title: Bit-rotted text adventure EDSL free to a good home
published: 2011-05-27T16:33:39Z
categories: haskell,projects
tags: adventure,EDSL,public domain,text
---

In early 2008 I started working on a Haskell embedded domain-specific language for authoring text adventure games.  It didn't get all that far since I didn't know a whole lot about either text adventure games or Haskell.  But I think there were some interesting ideas there: the most fundamental interesting idea is that everything is done with continuations, so you are not tied down to working within any particular framework.  Although the library provides combinators for working within a default "standard text adventure" framework (with locations, objects, an inventory, movement...) you are free to insert arbitrary Haskell at any point, say if you wanted to stick some little mini-game in the middle of your text adventure game, or whatever.

It has become apparent that I am never going to pick it up again, but it would be a shame to just let it rot on my hard drive.  So I've made the <a href="http://code.haskell.org/~byorgey/code/adventure/" title="Adventure repo">darcs repo publicly available</a>, licensed under a <a href="http://creativecommons.org/publicdomain/zero/1.0/legalcode">Creative Commons CC0 license.</a> If you are at all interested feel free to fork off your own copy and play around with it (your copy, of course, does not have to be public domain). I guarantee it will not compile but it probably wouldn't be that hard to get it working again.

