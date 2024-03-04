---
katex: true
title: 'Functional MetaPost'
published: 2009-09-22T03:25:14Z
categories: haskell
tags: errors,funcmp,MetaPost
---

I just spent a very frustrating hour trying to get the <a href="http://cryp.to/funcmp/">functional MetaPost library</a> (a Haskell DSL for generating <a href="http://www.tug.org/metapost.html">MetaPost</a>) working.  It installs (via <a href="http://haskell.org/haskellwiki/Cabal-Install">cabal-install</a>) without a hitch, but I haven't been able to get it to actually work.  I can write a test program which imports the FMP module, and compile it, but then when I try to run it, it spews out a gazillion .log, .tmp, .mpx, .aux... files and a nasty-looking series of error messages (including a segfault).  I'm happy to provide more details if anyone thinks they would actually be able to help...

All the more motivation to get back to a rewrite of my <a href="http://code.haskell.org/diagrams/">diagrams library</a>...

