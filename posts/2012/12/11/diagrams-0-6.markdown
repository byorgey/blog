---
title: Diagrams 0.6
published: 2012-12-12T02:32:05Z
categories: haskell,projects
tags: diagrams,EDSL,graphics,release
---

<p>I am pleased to announce the release of version 0.6 of <a href="http://projects.haskell.org/diagrams">diagrams</a>, a full-featured framework and embedded domain-specific language for declarative drawing. Check out the <a href="http://projects.haskell.org/diagrams/gallery.html">gallery</a> for examples of what it can do!</p>

<div style='text-align:center;'>

<p><a href="http://mathlesstraveled.com/2012/11/05/more-factorization-diagrams/"><img src="http://mathlesstraveled.files.wordpress.com/2012/11/77455a72e05ff905c9c81cd250b45fa6.png" /></a></p>
<p><a href="http://projects.haskell.org/diagrams/gallery/PythagoreanTree.html"><img src="http://projects.haskell.org/diagrams/gallery/PythagoreanTree.png" /></a></p>

</div>

<p>Highlights of this release include:</p>
<ul>
<li><p>Diagrams now comes with a native-Haskell <a href="http://hackage.haskell.org/package/diagrams%2Dsvg">SVG backend</a> by default. If you were holding off on trying diagrams because you couldn’t install cairo, you no longer have an excuse!</p></li>
<li><p>Proper support for subdiagrams: previous versions of diagrams-core had a mechanism for associating names with a pair of a location and an envelope. Now, names are associated with actual subdiagrams (including their location and envelope, along with all the other information stored by a diagram). This enables cool techniques like constructing a diagram in order to position its subelements and then taking it apart again, or constructing animations via keyframing.</p></li>
<li><p>Traces: in addition to an envelope, each diagram now stores a “trace”, which is like an embedded raytracer: given any ray (represented by a base point and a vector), the trace computes the closest point of intersection with the diagram along the ray. This is useful for determining points on the boundary of a diagram, <em>e.g.</em> when drawing arrows between diagrams.</p></li>
<li><p>The core data structure underlying diagrams has been completely refactored and split out into its own separate package, <a href="http://hackage.haskell.org/package/dual%2Dtree">dual-tree</a>.</p></li>
<li><p>Support for GHC 7.6.</p></li>
<li><p>Many more new features, bug fixes, and improvements! See the <a href="http://projects.haskell.org/diagrams/releases.html">release notes</a> for complete details, and the <a href="http://www.haskell.org/haskellwiki/Diagrams/Migrate0.6">diagrams wiki</a> for help migrating from 0.5 to 0.6.</p></li>
</ul>
<h2 id="try-it-out">Try it out</h2>
<p>For the truly impatient:</p>
<pre><code>cabal install diagrams</code></pre>
<p>Diagrams is supported under GHC 7.0 through 7.6, with the exception that the cairo and gtk backends do not build under GHC 7.0 (but the SVG backend does), and the gtk backend does not build under GHC 7.6.</p>
<p>To get started with diagrams, read the <a href="http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html">quick tutorial</a>, which will introduce you to the fundamentals of the framework.</p>
<p>For those who are less impatient and want to really dig in and use the power features, read the <a href="http://projects.haskell.org/diagrams/manual/diagrams-manual.html">user manual</a>.</p>
<h2 id="get-involved">Get involved</h2>
<p>Subscribe to the <a href="http://groups.google.com/group/diagrams-discuss">project mailing list</a>, and/or come hang out in the <code>#diagrams</code> IRC channel on freenode.org for help and discussion. Make some diagrams. <a href="http://github.com/diagrams/">Fix some bugs</a>. Submit your cool examples for inclusion in the <a href="http://projects.haskell.org/diagrams/gallery.html">gallery</a> or your cool code for inclusion in the <a href="http://hackage.haskell.org/package/diagrams%2Dcontrib">diagrams-contrib</a> package!</p>
<p>Happy diagramming!</p>
<p>Brought to you by the diagrams team:</p>
<ul>
<li>Michael Sloan</li>
<li>Ryan Yates</li>
<li>Brent Yorgey</li>
</ul>
<p>with contributions from:</p>
<ul>
<li>Sam Griffin</li>
<li>Niklas Haas</li>
<li>Peter Hall</li>
<li>Claude Heiland-Allen</li>
<li>Deepak Jois</li>
<li>John Lato</li>
<li>Felipe Lessa</li>
<li>Chris Mears</li>
<li>Ian Ross</li>
<li>Vilhelm Sjöberg</li>
<li>Jim Snavely</li>
<li>Luite Stegeman</li>
<li>Kanchalai Suveepattananont</li>
<li>Michael Thompson</li>
<li>Scott Walck</li>
</ul>

