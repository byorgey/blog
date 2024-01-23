---
title: Diagrams 1.0
published: 2014-01-17T12:23:41Z
categories: diagrams,haskell,projects
tags: 1.0,announcement,release
---

<p>The diagrams team is very pleased to announce the 1.0 release of <a href="http://projects.haskell.org/diagrams">diagrams</a>, a framework and embedded domain-specific language for declarative drawing in Haskell. Check out the <a href="http://projects.haskell.org/diagrams/gallery.html">gallery</a> for some examples of what it can do. Diagrams can be used for a wide range of purposes, from <a href="http://idontgetoutmuch.wordpress.com/2013/10/23/parking-in-westminster-an-analysis-in-haskell/">data visualization</a> to <a href="https://www.fpcomplete.com/user/edwardk/cellular-automata/part-1">illustration</a> to <a href="http://mathlesstraveled.com/2013/04/06/stars-of-the-minds-sky-with-diagrams/">art</a>, and diagrams code can be seamlessly embedded in <a href="http://byorgey.wordpress.com/2012/08/28/creating-documents-with-embedded-diagrams/">blog posts</a>, <a href="http://projects.haskell.org/diagrams/doc/latex.html">LaTeX documents</a>, and <a href="http://byorgey.wordpress.com/2013/03/23/introducing-diagrams-haddock/">Haddock documentation</a>, making it easy to incorporate diagrams into your documents with minimal extra work.</p>
<div style="text-align:center;">
<a href="http://projects.haskell.org/diagrams/gallery/Chart.html"><img src="http://projects.haskell.org/diagrams/gallery/images/Chart.thumb.png" /></a>     <a href="http://projects.haskell.org/diagrams/gallery/KnightTour.html"><img src="http://projects.haskell.org/diagrams/gallery/images/KnightTour.thumb.png" /></a>     <a href="http://projects.haskell.org/diagrams/gallery/SquareLimit.html"><img src="http://projects.haskell.org/diagrams/gallery/images/SquareLimit.thumb.png" /></a>
</div>
<h2 id="whats-new">What’s new</h2>
<p>Brent recently gave a talk at the <a href="http://www.meetup.com/NY-Haskell/">New York Haskell Users’ Group</a> presenting the new release. You can find videos of the talk on vimeo: <a href="http://vimeo.com/84104226">part 1 presents a basic introduction to the library</a>, and <a href="http://vimeo.com/84249042">part 2 talks about mathematical abstraction and DSL design</a>. The <a href="http://www.cis.upenn.edu/~byorgey/pub/13-11-25-nyhaskell-diagrams.pdf">slides are also available</a>.</p>
<p>This release includes a number of significant new features and improvements. Highlights include:</p>
<ul>
<li><p>Support for drawing arrows between given points or between diagrams, with many options for customization (<a href="http://projects.haskell.org/diagrams/doc/arrow.html">tutorial</a>, <a href="http://projects.haskell.org/diagrams/doc/manual.html#arrows">documentation</a>, <a href="http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Arrow.html">API</a>).</p></li>
<li><p>A new framework for creating custom command-line-driven executables for diagram generation (<a href="http://projects.haskell.org/diagrams/doc/cmdline.html">tutorial</a>, <a href="http://projects.haskell.org/diagrams/haddock/Diagrams-Backend-CmdLine.html">API</a>).</p></li>
<li><p>Offsets of trails and paths, <em>i.e.</em> compute the trail or path lying a constant distance from the given one (<a href="http://projects.haskell.org/diagrams/doc/manual.html#offsets-of-segments-trails-and-paths">documentation</a>, <a href="http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Offset.html">API</a>).</p></li>
<li><p>A new API, based on Metafont, for constructing cubic splines with control over things like tangents and “tension” (<a href="http://projects.haskell.org/diagrams/doc/metafont.html">tutorial</a>, <a href="http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Path-Metafont.html">API</a>).</p></li>
<li><p>Tangent and normal vectors of segments and trails (<a href="http://projects.haskell.org/diagrams/haddock/Diagrams-Tangent.html">API</a>).</p></li>
<li><p>Alignment can now be done by trace in addition to envelope (<a href="http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Align.html">API</a>).</p></li>
<li><p>The <a href="http://hackage.haskell.org/package/lens"><code>lens</code></a> package is now used consistently for record fields throughout the library (<a href="http://projects.haskell.org/diagrams/doc/manual.html#faking-optional-named-arguments">documentation</a>).</p></li>
<li><p>Across-the-board improvements in performance and size of generated files.</p></li>
</ul>
<p>See the <a href="http://projects.haskell.org/diagrams/releases.html">release notes</a> for full details, and the <a href="http://www.haskell.org/haskellwiki/Diagrams/Dev/Migrate1.0">migration guide</a> for help porting your diagrams 0.7 code to work with diagrams 1.0.</p>
<h2 id="try-it-out">Try it out</h2>
<p>For the truly impatient:</p>
<pre><code>cabal install diagrams</code></pre>
<p>Diagrams is supported under GHC 7.4 and 7.6.</p>
<p>To get started, read the <a href="http://projects.haskell.org/diagrams/doc/quickstart.html">quick start tutorial</a>, which will introduce you to the fundamentals of the framework and provide links for further reading.</p>
<p>For those who are less impatient and want to really dig in and use the power features, read the extensive <a href="http://projects.haskell.org/diagrams/doc/manual.html">user manual</a>. There is also a growing <a href="http://projects.haskell.org/diagrams/documentation.html">collection of tutorials</a> on specific topics.</p>
<h2 id="get-involved">Get involved</h2>
<p>Diagrams has a friendly and growing community of users and developers. To connect with the community, subscribe to the <a href="http://groups.google.com/group/diagrams-discuss">project mailing list</a>, and/or come hang out in the <code>#diagrams</code> IRC channel on freenode.org for help and discussion. Development continues stronger than ever, and there are a wide range of projects available for new contributors of all levels of Haskell skill. Make some diagrams. <a href="http://github.com/diagrams/">Fix some bugs</a>. Submit your cool examples for inclusion in the <a href="http://projects.haskell.org/diagrams/gallery.html">gallery</a> or your cool code for inclusion in the <a href="http://hackage.haskell.org/package/diagrams%2Dcontrib">diagrams-contrib</a> package.</p>
<p>Happy diagramming!</p>
<p>Brought to you by the diagrams team:</p>
<ul>
<li>Daniel Bergey</li>
<li>Jeff Rosenbluth</li>
<li>Ryan Yates</li>
<li>Brent Yorgey</li>
</ul>
<p>with contributions from:</p>
<ul>
<li>Jan Bracker</li>
<li>Conal Elliott</li>
<li>Daniil Frumin</li>
<li>Sam Griffin</li>
<li>Niklas Haas</li>
<li>Peter Hall</li>
<li>Claude Heiland-Allen</li>
<li>Deepak Jois</li>
<li>John Lato</li>
<li>Felipe Lessa</li>
<li>Chris Mears</li>
<li>Ian Ross</li>
<li>Carlos Scheidegger</li>
<li>Vilhelm Sjöberg</li>
<li>Michael Sloan</li>
<li>Jim Snavely</li>
<li>Luite Stegeman</li>
<li>Kanchalai Suveepattananont</li>
<li>Michael Thompson</li>
<li>Scott Walck</li>
</ul>
<div class="references">

</div>

