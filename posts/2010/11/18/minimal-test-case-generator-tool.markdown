---
title: 'Minimal test case generator tool?'
published: 2010-11-18T22:28:27Z
categories: meta
tags: automatic,case,generator,minimal,shrinking,test,testing
---

An interesting idea that <a href="http://www.dmwit.com/">Daniel Wagner</a>, <a href="http://www.seas.upenn.edu/~vilhelm/">Vilhelm Sjöberg</a> and I just came up with: a <i>minimal test case generator</i>.  Here's the idea: you have a program which exhibits error X, which you want to understand better; or perhaps you suspect it is a bug in the compiler, and you want to submit a test case along with your bug report.  As input to the minimal test case generator, you give your program along with some sort of description of error X.  It then tries to shrink your program as much as possible while still exhibiting error X.  Of course, doing the shrinking would be nontrivial: you'd have to parse the program and do some sort of call graph analysis, and then only delete leaves in the call graph.  But, of course, tools for analyzing call graphs <a href="http://hackage.haskell.org/package/SourceGraph">already exist</a>.

How difficult would this be to develop?  Does anything like this already exist?

