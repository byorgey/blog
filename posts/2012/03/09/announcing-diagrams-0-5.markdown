---
title: Announcing diagrams 0.5
published: 2012-03-09T18:50:13Z
categories: haskell,projects
tags: diagrams,EDSL,graphics,release,vector
---

<p>I am pleased to announce the release of version 0.5 of <a href="http://projects.haskell.org/diagrams">diagrams</a>, a full-featured framework and embedded domain-specific language for declarative drawing. Check out the <a href="http://projects.haskell.org/diagrams/gallery.html">gallery</a> for examples of what it can do!</p>
<div align="center">
<a href="http://projects.haskell.org/diagrams/gallery/FibCalls.html"><img src="http://projects.haskell.org/diagrams/gallery/FibCalls.svg" width="400" alt="Naive fibonacci call tree" /></a>
</div>
<p>Highlights of this release include:</p>
<ul>
<li>A new <a href="http://hackage.haskell.org/package/diagrams%2Dcontrib">diagrams-contrib</a> package of user-contributed modules, which so far contains code for tree drawing, Apollonian gaskets, planar tilings, &quot;wrapped&quot; layout, and turtle graphics.</li>
<li>Experimental support for animation, built on top of the new <a href="http://hackage.haskell.org/package/active">active</a> library.</li>
<li>Numerous small additions and improvements, including more general rounded rectangle shapes and better text support.</li>
<li>Much better performance in some common situations, such as laying out a very long list of diagrams using 'cat' and related combinators.</li>
<li>Added support for GHC 7.4.</li>
</ul>
<p>See the <a href="http://projects.haskell.org/diagrams/releases.html">release notes</a> for complete details, and the <a href="http://www.haskell.org/haskellwiki/Diagrams/Migrate0.5">diagrams wiki</a> for help migrating code from 0.4 to 0.5 (changes should be minimal).</p>
<h2 id="try-it-out">Try it out</h2>
<p>For the truly impatient:</p>
<pre><code>cabal install gtk2hs-buildtools
cabal install diagrams</code></pre>
<p>Diagrams is supported under GHC 6.12, 7.0, 7.2, and 7.4. However, getting cairo to build can be tricky on some platforms; see the <a href="http://www.haskell.org/haskellwiki/Diagrams">diagrams wiki</a> for more information and workarounds regarding specific platforms. (A new native SVG backend is in the works, targeted for the 0.6 release.)</p>
<p>To get started with diagrams, read the <a href="http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html">quick tutorial</a>, which will introduce you to the fundamentals of the framework.</p>
<p>For those who are even less impatient but want to really dig in and use the power features, read the <a href="http://projects.haskell.org/diagrams/manual/diagrams-manual.html">user manual</a>.</p>
<h2 id="get-involved">Get involved</h2>
<p>Subscribe to the <a href="http://groups.google.com/group/diagrams-discuss">project mailing list</a>, and/or come hang out in the <code>#diagrams</code> IRC channel on freenode.org for help and discussion. Make some diagrams. <a href="http://code.google.com/p/diagrams/issues/list">Fix some bugs</a>. Submit your cool examples for inclusion in the <a href="http://projects.haskell.org/diagrams/gallery.html">gallery</a> or your cool code for inclusion in the <a href="http://hackage.haskell.org/package/diagrams%2Dcontrib">diagrams-contrib</a> package!</p>
<p>Happy diagramming!</p>
<p>Brought to you by the diagrams team:</p>
<ul>
<li>Peter Hall</li>
<li>Ian Ross</li>
<li>Michael Sloan</li>
<li>Ryan Yates</li>
<li>Brent Yorgey</li>
</ul>
<p>with contributions from:</p>
<ul>
<li>Sam Griffin</li>
<li>Claude Heiland-Allen</li>
<li>John Lato</li>
<li>Vilhelm Sjöberg</li>
<li>Luite Stegeman</li>
<li>Kanchalai Suveepattananont</li>
<li>Scott Walck</li>
</ul>


