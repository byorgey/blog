---
title: QuickCheck rocks my socks
published: 2008-12-17T03:59:43Z
categories: haskell,projects
tags: infinite recursion,QuickCheck
---

Over the past few days I've been hacking on a <a href="http://haskell.org/haskellwiki/Data.List.Split">Data.List.Split module</a>, to be used when you just want to quickly split up a list without going to the trouble of using a real parsing or regular expression library -- for example, suppose you are writing a one-off script that needs to read in strings like "abc;def;gh;i" and you want to split it on the semicolons to yield a list of Strings.  Of course, such a thing isn't in the standard libraries since no one can agree on the right interface; the idea is to provide a whole module with lots of different ways to split instead of a single function, and to just put it on <a href="http://hackage.haskell.org">Hackage</a> instead of going through the much more difficult process of getting it included in the standard libraries.  Anyway, more on that when it's released, hopefully in a few days.

Like any good Haskell programmer writing a nice pure library, today I started adding a suite of <a href="http://www.cs.chalmers.se/~rjmh/QuickCheck/">QuickCheck</a> properties.  I set up a framework and added a couple basic properties: 200 tests passed!  I added another: 100 tests passed!  Now I was on a roll, and added three more.  This time... it hung after checking the fourth property only 8 times.  OK, no problem, I've seen this sort of thing before when there's some sort of combinatorial explosion in the size of the randomly generated test data... except the test data is so simple that's definitely not what's happening here.  Hmm... maybe it's infinite recursion?  But I really can't see where infinite recursion could crop up.  Oh, unless... hmm, yes, if function A ever returns an empty list, it would cause an infinite loop.  But surely function A can never return an empty list!  Well, let's try it.  prop_A_nonempty x = (not . null) (funcA x).  And... Falsified!  Whoops.

If you're curious, the case I forgot was when you specify the empty list as a delimiter -- obvious in retrospect, perhaps, but without QuickCheck's assistance, I probably would have ended up releasing the library with this latent infinite recursion bug!

