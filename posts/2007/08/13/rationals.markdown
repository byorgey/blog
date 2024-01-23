---
title: 'Rationals!'
published: 2007-08-13T19:37:06Z
categories: haskell,math
tags: 
---

Inspired by this <a href="http://www.lacim.uqam.ca/~plouffe/OEIS/citations/recounting.pdf">totally sweet paper by Calkin &amp; Wilf</a> (read it for more details, it's very short and quite elegantly written) (no really, you should read it):
<pre>
import Data.Ratio

buildHB (x1:x2:xs) = (x1 + x2) : x1 : buildHB (x2:xs)
hypbin = 1 : 1 : buildHB hypbin
rationals = zipWith (%) hypbin (tail hypbin)</pre>
How cool is that?  The infinite list of all positive rational numbers, each occurring exactly once in reduced form.  In only three lines of Haskell.  There are actually better ways to do this; e.g. see the <a href="http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/rationals.pdf">functional pearl by Gibbons, Lester, and Bird</a>.  In particular the three-line version above uses O(n) memory to generate the first n rationals, since half of the hypbin list has to be kept around to generate the rest of it, whereas it can actually be done in constant memory.  But it's still nifty.

