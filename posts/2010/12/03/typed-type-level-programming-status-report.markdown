---
katex: true
title: 'Typed type-level programming: status report'
published: 2010-12-03T21:51:31Z
categories: haskell,projects
tags: extension,GHC,hacking,lifting,progress,refactoring,types
---

A few people have been bugging me (you know who you are ;-) about the current status of the project to allow automatic "lifting" of Haskell data constructors to the type level, to allow for <i>typed</i> type-level programming.  I've written about it on this blog before (see <a href="http://byorgey.wordpress.com/2010/07/19/typed-type-level-programming-in-haskell-part-iii-i-can-haz-typs-plz/">here</a> and <a href="http://byorgey.wordpress.com/2010/08/05/typed-type-level-programming-in-haskell-part-iv-collapsing-types-and-kinds/">here</a>) and gave a talk about it in September at the Haskell Implementors' Workshop (<a href="http://vimeo.com/15480577">video</a>, <a href="http://www.cis.upenn.edu/~byorgey/talks/typetype-HIW-20101001.pdf">slides</a>).  So, there's bad news, and there's good news.

The bad news is that it is still pretty much vaporware (or vapourware I suppose) -- there is not yet any actual code anywhere to do any actual automatic lifting!  The good news is that it is still definitely being worked on.  We (being Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Steve Zdancewic, and I) spent a bunch of time this summer trying to work out the theory of this extension, and I think we have some solid ideas.  I then spent quite a lot of time refactoring GHC to make coercions a separate type from types (another interesting story in and of itself) which simply had to be done before we could make progress on this extension.  Due to the magnitude of the task, some problems with darcs, and plain old busyness, that refactoring work has only recently been fully merged with the current GHC HEAD, although it is not yet pushed to the HEAD since there are still some kinks to iron out!  But once that is done we can get started with the work to actually implement auto-lifting.  

