---
title: Math.OEIS needs a new maintainer
published: 2010-12-04T21:32:01Z
categories: haskell,math,projects
tags: maintainer,new,OEIS,seeking,upgrade
---

<i>Summary: the <a href="http://oeis.org/">OEIS</a> has switched servers and the <a href="http://hackage.haskell.org/package/oeis">oeis</a> package needs updating to match, so I'm looking for someone to take over development.  All the rest is padding in the form of entertaining stories.</i>

Recently, <a href="http://www.snoyman.com/">Michael Snoyman's</a> wonderful <a href="http://packdeps.haskellers.com/">packdeps tool</a> notified me that there were some updated dependencies for my <a href="http://hackage.haskell.org/package/oeis">oeis</a> package, a Haskell interface to Neil Sloane's <a href="http://oeis.org/">Online Encyclopedia of Integer Sequences</a>.  In particular, there was a newer version of <a href="http://hackage.haskell.org/package/network">network</a> available. So I installed the newest version of network, recompiled the package with no errors or warnings, and tried looking up the prime numbers --- and got <code>Nothing</code>!  I tried a few other lookups with the same result.  Strangely enough, downgrading network and recompiling did not help matters.  Lambdabot's @oeis command (which uses oeis as a plugin) was also failing to obtain any results.

After a bit of poking around, I found the real problem --- the OEIS has recently moved to a different server, using different software!  The queries were all failing because they were querying the wrong thing.

Amusingly, while poking around the new OEIS trying to figure out how it is organized, I <a href="http://oeis.org/wiki/Welcome#Policy_on_Searching_the_Database">found this</a>:
<blockquote>
Just as it is OK for a browser (such as Firefox) to access the OEIS, so it is also OK for a computer algebra program such as SAGE or <strong>Haskell</strong> to have an interface with the OEIS, provided of course that this does not put too much of a burden on the server here.
</blockquote>
That's right, Neil Sloane himself has given his <i>explicit blessing</i> to the Haskell oeis package!  I guess we can forgive the fact that he thinks Haskell is a "computer algebra package".

Unfortunately, the way that OEIS queries work and the formats in which you can retrieve data seem to have changed (and it looks like they might change again!), so getting the oeis package to work again is not just a simple matter of updating a URL.  I don't really have much time to spend on it right now, so I'm looking for someone to take over its development and maintenance.  In addition to simply getting it to work again, there are a number of features I've thought about adding but never got around to; the biggest one being the ability to return multiple search results to the user instead of just the first.  So perhaps that's something a new maintainer could work on as well.  If you're interested, please send me an email!

