---
katex: true
title: '2009 ICFP programming contest reflections'
published: 2009-06-29T21:41:20Z
categories: haskell,learning,puzzle
tags: contest,ICFP,mechanics,orbital,programming,satellites
---

This year, unlike <a href="https://byorgey.github.io/blog/posts/2008/07/15/icfp-programming-contest-reflections.html">last year</a>, I had the good fortune to be physically located in the same place as several other people interested in competing in the <a href="http://icfpcontest.org/">2009 ICFP Programming Contest</a>.  We only ended up with three people---we could have used more---but it was quite a lot of fun.

The problem this year (controlling some satellites to accomplish certain goals) was a good one.  I especially liked the approach of distributing binaries to be run on a virtual machine, and requiring execution traces as submissions---as opposed to last year's contest, it removed all the ickiness with wrangling execution platforms and ensuring your code would run on the organizers' servers.  And the problem itself was nifty, easy to get started on, difficult to finish, and always left room for improvements.  My only complaint is that I would have been more excited about something with a more discrete flavor, since this was the second contest in a row with a simulation of physical objects, vector math, trig, and so on---but that's only a small complaint.  We did respectably, solving 8 of the 16 scenarios (100x and 200x)---although of course hindsight is 20/20, and I think in particular we could have pretty easily solved all the 300x scenarios.  We ended up spending too much time trying to work things out mathematically and not enough just doing simulation and search (although I'm definitely biased since I'm the one who wrote a physics simulator module... =).

We used Haskell, of course, which seemed to mostly work well.  I wrote our VM, and although it wasn't blazing fast, it was fast enough for what we ended up doing with it. The only truly annoying part was serializing and deserializing IEEE doubles, since Data.Binary doesn't use IEEE format, and I didn't know about the data-binary-ieee754 package until too late.  I ended up doing some ugliness involving ByteStrings, foreign pointers, peek and poke... yuck!

The first scenario was easy enough, using the provided information about Hohmann transfer orbits and looking up some math on computing the required vectors (my teammate Julien did most of the heavy lifting mathematics-research-wise... I just typed what he told me to =).  For the second scenario, we were able to figure out how to compute the correct angle at which to initiate a Hohmann transfer in order to meet up with the second satellite, but the problem at first was that it wasn't accurate enough---but we finally got it to work doing some simple searches with a physics simulator (after I spent two hours figuring out that negating an angle is NOT the same thing as adding pi to it!).  I'm confident we could have gotten the third scenario to work similarly, by first transferring to a circular orbit tangent to an apogee of the target orbit (doing some sort of calculation/simulation to work out the timing correctly), but oh well, we didn't get there.  This is also where having more people would have helped, both to be able to code more stuff in parallel and for some fresh ideas.

But, all in all, a most enjoyable experience, and I look forward to putting together a (hopefully larger) team again next summer!

