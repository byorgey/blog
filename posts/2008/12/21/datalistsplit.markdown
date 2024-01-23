---
title: Data.List.Split
published: 2008-12-21T19:13:28Z
categories: haskell,projects
tags: library,list,split
---

Have you ever had a string like this
<code>
"abc;def;ghijk;lm"
</code>
and wanted to turn it into a list of strings, like this?
<code>
["abc", "def", "ghijk", "lm"]
</code>
Of course, you could always use a parsing library, or a regular expression library, but sometimes you just want something a little more lightweight.  Perl and Ruby both have library functions called "split" to do just this.  Haskell's standard libraries, on the other hand, have no such function, much to the consternation of many a newbie and experienced Haskeller alike.  There have been <a href="http://www.haskell.org/pipermail/libraries/2006-July/005504.html">many</a> <a href="http://www.haskell.org/pipermail/libraries/2006-October/006072.html">proposals</a> to <a href="http://www.haskell.org/pipermail/libraries/2008-January/008922.html">add</a> such a thing to the standard Data.List module in the past, but nothing ever came of it, primarily because there are many slightly different ways to split a list, and no one could ever agree on the One True Splitting Interface.

I decided we've been Doing It Wrong.  Instead of bickering about the one true interface and going through the stringent library proposals process, let's just get some useful code together and release it on <a href="http://hackage.haskell.org">Hackage</a>.  (Of course there are advantages to inclusion in the standard libraries --- but that can come later.)  So I solicited contributions on a <a href="http://haskell.org/haskellwiki/Data.List.Split">wiki page</a>, took some of the ideas, bits of code, and some ideas of my own, and created <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/split">Data.List.Split</a>.

Instead of talking about it more, I'll just show some examples:

<code>
*Data.List.Split&gt; splitOn ";" "abc;def;ghijk;lm"
["abc","def","ghijk","lm"]
*Data.List.Split&gt; splitWhen (&lt;0) [1,4,-8,4,-3,-2,9]
[[1,4],[4],[],[9]]
*Data.List.Split&gt; split (startsWith "app") "applyappicativeapplaudapproachapple"
["apply","appicative","applaud","approach","apple"]
*Data.List.Split&gt; split (dropDelims $ oneOf ":;") "::abc;:;;fg:h;;ij;"
["","","abc","","","","fg","h","","ij",""]
*Data.List.Split&gt; split (condense . dropInitBlank $ oneOf ":;") "::abc;:;;fg:h;;ij;"
["::","abc",";:;;","fg",":","h",";;","ij",";",""]
</code>

Detailed documentation can be found in the package itself.  Install it from Hackage:
<code>
cabal install split
</code>

You can also check out the <a href="http://code.haskell.org/~byorgey/code/split">darcs repo</a>.  Comments, suggestions, and patches welcome!

