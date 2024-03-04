---
katex: true
title: 'Hac φ roundup'
published: 2009-07-29T16:45:13Z
categories: haskell,meta
tags: Hac φ,hackathon,projects,roundup
---

By all accounts, Hac φ was a great success!  25 people attended (from as far away as Chicago, Toronto, and Utrecht) and we had a great time hanging out, eating good food, listening to some interesting talks, and, oh yes, hacking.  There was a wide range of Haskell experience represented among the participants, which made for a great atmosphere of collaboration and learning.  A collection of my pictures can be <a href="http://yorgeys.smugmug.com/gallery/9039600_Ra9wn/1/601548486_mPvwh">found here</a>; Shae's pictures can be found <a href="http://picasaweb.google.com/shae.erisson/HacPhiDayMinusOne#">here</a>, <a href="http://picasaweb.google.com/shae.erisson/HacPhiDayTwo#">here</a>, <a href="http://picasaweb.google.com/shae.erisson/HacPhiDayTwo02#">here</a>, and <a href="http://picasaweb.google.com/shae.erisson/HacPhiDayThree#">here</a>.  Unfortunately there don't seem to be any pictures of when we went out for dinner and beer, but I can assure you that much fun was had by all.  I also have <a href="http://byorgey.wordpress.com/2009/07/25/more-from-hac-%cf%86/">another post describing the talks</a> on Saturday afternoon.

Here's a quick rundown of some of the things people worked on (by no means complete):

<ul>
	<li>Chris Eidhof (chr1s) and doug Beardsley (mightybyte) worked on the <a href="http://hackage.haskell.org/package/formlets">formlets</a> library, making a bunch of internal changes.</li>
	<li>Tracy Wadleigh (twadleigh) worked on his <a href="http://hackage.haskell.org/package/mathlink">mathlink</a> library for implementing Mathematica packages in Haskell.</li>
	<li>Greg Collins (gcollins) made a number of improvements and bugfixes to the OSX installer for the <a href="http://hackage.haskell.org/platform/">Haskell Platform</a>.</li>
	<li>Chris Casinghino (ccasin) worked on creating some open-source tools for crossword puzzle construction and solving.</li>
	<li>Edward Kmett (edwardk) and Shae Erisson (shapr) worked on Edward's small, mostly untyped, lazy functional programming language Kata; Shae also did a bit of work on parsing FLAC files.</li>
	<li>Ravi Nanavati (ravi_n) collaborated with Edward Kmett to design a monoid for error message collection and started planning for a Boston hackathon in January (?), among other things.</li>
	<li>Mike Burns (mike-burns) finished a minimally working program called "taggert" that provides for a <a href="http://mike-burns.com/project/taggert/taggert%20-%200.01%20preview.png">tagged view of a directory</a>, using gtk2hs and HDBC-Sqlite3.</li>
	<li>Gershom Bazerman (sclv) worked on releasing his <a href="http://hackage.haskell.org/package/jmacro">jmacro library</a> for programmatic JavaScript generation.</li>
	<li>Dan DaCosta (chaosape) worked on learning Haskell and designing a library for collection, aggregation, and analysis of network traffic, to be used for his Master's thesis.</li>
	<li>Daniel Wagner (dmwit) worked on a library for parsing .sgf files.</li>
	<li>Yours truly (byorgey) worked on a new splitting method for my <a href="http://hackage.haskell.org/package/split">Data.List.Split</a> library, applied a few <a href="http://www.xmonad.org/">xmonad</a> patches, and worked a bunch on my new <a href="http://hackage.haskell.org/package/species">combinatorial species library</a>.
</ul>

Those are the projects I know about; there were also a number of other attendees, whose accomplishments were surely no less interesting even though I don't know what they were: Adam Turoff (\z), Anton van Straaten (mmmdonuts), Luke Hoersten (Luke), Reece Heineke, Sasha Rush, Malik Hamro (malikh), Andrew Robbins (adu), Andrew Wagner (chessguy), Aleks Jakulin (AleksJ), Hristo Asenov (hasenov), and Ken Takusagawa.  Please feel free to leave comments with information about projects I left out, more pictures, blog posts, or whatever!

Towards the end of the hackathon several people asked me whether we would be hosting another one next year.  We hadn't planned that far ahead, of course---but judging by how much fun this one was (and how unstressful it was to organize (although that was largely thanks to our amazing administrative coordinator, Cheryl Hickey, who handled a lot of the details!)), it's a distinct possibility!  In fact, since it looks like ICFP will be in Baltimore next year, perhaps we can help organize another hackathon in conjunction with that.  We shall see!

