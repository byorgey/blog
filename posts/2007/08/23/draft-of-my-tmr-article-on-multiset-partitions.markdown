---
title: Draft of my TMR article on multiset partitions
published: 2007-08-23T11:43:24Z
categories: haskell,writing
tags: 
---

Here's a draft of the article I'm writing for hopeful inclusion in the upcoming issue of <a href="http://www.haskell.org/haskellwiki/The_Monad.Reader" target="_blank">The Monad.Reader</a>:

[<b>EDIT</b>, 11/10/2007: I've removed the draft version since it seems that people are finding it from searches and reading it without realizing it's a draft.  I'd much rather people read the (much better) version that was actually published in the Monad.Reader: you can find it <a href="http://haskell.org/sitewiki/images/d/dd/TMR-Issue8.pdf">here, in Issue #8 of the Monad.Reader</a>.  The literate Haskell source <a href="http://sneezy.cs.nott.ac.uk/darcs/TMR/Issue8/Partitions.lhs">can be found here</a>.]

I'd be extremely grateful to anyone who felt inclined to read it and offer comments (it's about 12 pages).  I'm particularly interested in whether it is (in your opinion) clear, and whether it is written at an appropriate level, or if it could use more/less explanation; but of course I'd be grateful for feedback of any sort.  I'm also curious whether anyone can point to anything related in the literature.  I haven't turned anything up but I can't imagine this has never been done before.

To give a very brief synopsis, the article starts out with the problem of efficiently enumerating all the partitions of a multiset -- for example, [1,1,2] has four unique partitions: [[[1,1,2]], [[1,1],[2]], [[1,2], [1]], [[1],[1],[2]]].  I then develop some Haskell code to enumerate vector partitions, and use this to compute multiset partitions by representing multisets as vectors of element counts.  That's the bare-bones outline, although there's more to it than that, of course -- in particular the vector partition code ends up having a few additional applications as well.  I think it's neat and (mostly) pretty elegant.

So, thanks in advance to anyone who takes the time to offer me some feedback!  Maybe someday I'll get a chance to bake you some cookies.  Or something.

