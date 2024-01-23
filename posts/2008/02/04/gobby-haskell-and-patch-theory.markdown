---
title: Gobby, Haskell, and patch theory
published: 2008-02-04T22:24:16Z
categories: haskell,projects
tags: collaborative editing,gobby,patch theory
---

On a couple of occasions over the past few days, I've had the pleasure of editing code with other #haskellers, using the collaborative text editor <a href="http://gobby.0x539.de/trac/">gobby</a>.  It was quite a lot of fun, and (somewhat surprisingly, to me at least) rather effective.  It could stand a good chance of becoming one of my preferred methods for collaborative programming.

Except.

Except that gobby doesn't support undo!  It doesn't even have a way to go "back in time" to a previous version of a document (unless you have it saved locally, of course).  This is, quite plainly, a huge show-stopper, and I don't say that just in theory: I ran into this issue head-on after only a few hours of use.  I wanted to delete a bunch of double quotes, so did a search-and-replace.  It was taking too long to click "replace" for each double quote character, so I clicked "replace all", thinking it would only replace double quotes from the current location to the end of the file.  Well, as you can probably guess, it deleted all the double quotes everywhere in the file.  Oops. Ten minutes of tracking down missing quotes ensued, and I don't even want to imagine what it would have been like without the help of the typechecker.

This was ten minutes of wasted time.  I should have been able to just hit "undo" and get on with life.

So, why doesn't gobby have undo?  I looked at their <a href="http://gobby.0x539.de/trac/report/1">bug tracker</a>, and sure enough, this has been a <a href="http://gobby.0x539.de/trac/ticket/39">requested</a> <a href="http://gobby.0x539.de/trac/ticket/234">feature</a> since 2005.  But it's never been done, since undo in a collaborative-editing context is a hard problem.  Apparently someone is sorta-kinda working on an implementation, that works in some cases...

A hard problem, eh?  What hard problems need is good theory.  Well, gee goshums!  If only there was some sort of <a href="http://en.wikibooks.org/wiki/Understanding_darcs/Patch_theory">theory of concurrent editing</a>... 

So, the upshot of all of this is that I am seriously considering writing a gobby clone in Haskell with some sweet, sweet patch theory goodness.  (Other killer features under possible consideration: hpaste and darcs integration, a Yi frontend, integrated compilation/error annotations, chat support that doesn't suck...) I do have a lot more thoughts on the particulars of how this might work, but I won't write more about the specifics here since that's not really the point of this post.

The point is that although I said "seriously considering", in actuality I'm not sure exactly how serious I really am.  This would be a pretty big project, and I certainly couldn't get anywhere close to pulling it off myself.  I'm not even sure that I understand all of what it would require; I'd be particularly clueless about the networking and UI stuff.  I just think that it would be pretty sweet, and a great application of Haskell's strengths. Mostly at this point I'm looking for feedback.  What do you think?  Is this an awesome idea?  Is it a stupid idea?  Most importantly, would you be interested in working on it?

