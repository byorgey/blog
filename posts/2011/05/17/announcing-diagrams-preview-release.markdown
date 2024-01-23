---
title: Announcing diagrams preview release
published: 2011-05-17T15:42:28Z
categories: haskell,projects
tags: announcement,diagrams,drawing,EDSL,preview,release
---

<p>I am extremely pleased to announce a &quot;developer preview&quot; release of the <a href="http://projects.haskell.org/diagrams/">diagrams framework</a> for declarative drawing. This is a well-thought-out, well-documented, working release with all core functionality in place, but with many planned features still missing (for example, support for rendering text and higher-level tools for constructing curves). If you are interested in</p>
<ul>
<li>trying out a new way of producing vector graphics,</li>
<li>providing feedback to help drive ongoing development, or</li>
<li>getting involved and contributing some code yourself,</li>
</ul>
<p>please give it a try! On the other hand, if you are looking for a complete, full-featured package that will let you jump right into producing the graphics you need, you may want to wait for the 1.0 release.</p>
<p>If you are familiar with the <a href="http://hackage.haskell.org/package/diagrams">diagrams package</a> already on Hackage, this is a complete rewrite which has been in the works for over a year and a half.</p>

<a href="http://byorgey.files.wordpress.com/2011/05/fractal.png"><img src="http://byorgey.files.wordpress.com/2011/05/fractal.png" alt="" title="Fractal" width="400" height="400" class="aligncenter size-full wp-image-588" /></a>
<h2 id="what-is-it">What is it?</h2>
<p>Diagrams is an embedded domain-specific library (EDSL) for creating diagrams, illustrations, and other sorts of vector graphics. The overall vision is for diagrams to become a viable alternative to systems like MetaPost, Asymptote, and PGF/TikZ.</p>
<p>Diagrams is:</p>
<ul>
<li><p><em>Declarative</em>: you specify <em>what</em> a diagram is, not <em>how</em> to draw it.</p></li>
<li><p><em>Compositional</em>: diagrams can be combined in many ways to produce more complex diagrams. Diagrams are scale- and translation-invariant, so you never have to worry about a &quot;global&quot; coordinate system, only &quot;local&quot; ones.</p></li>
<li><p><em>Embedded</em>: the full power of Haskell, including every library on Hackage, is available to help construct and manipulate diagrams.</p></li>
<li><p><em>Extensible</em>: extending diagrams with additional or higher-level functionality is as simple as writing a Haskell module.</p></li>
<li><p><em>Flexible</em>: diagrams is designed from the ground up to be as generic and flexible as possible. Features include:</p>
<ul>
<li><p>Pluggable rendering backends -- creating a new rendering backend is as simple as writing a type class instance.</p></li>
<li><p>Arbitrary vector spaces -- the core diagrams library data types and primitives work for any vector space, so given a suitable rendering backend you can produce diagrams of any dimension, or even more exotic things...</p></li>
</ul></li>
</ul>

<a href="http://byorgey.files.wordpress.com/2011/05/paradox1.png"><img src="http://byorgey.files.wordpress.com/2011/05/paradox1.png" alt="" title="paradox" width="400" height="200" class="aligncenter size-full wp-image-592" /></a>
<h2 id="cool-how-can-i-try-it-out">Cool, how can I try it out?</h2>
<p>Start by reading the <a href="http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html">quick tutorial</a>, which has detailed information about how to install the necessary packages and will introduce you to the fundamentals of the framework.</p>
<p>Or, for the truly impatient:</p>
<pre><code>cabal install diagrams-core diagrams-lib diagrams-cairo
</code></pre>
<h2 id="how-can-i-contribute">How can I contribute?</h2>
<p>There are lots of ways you can contribute! First, you may want to subscribe to the <a href="http://groups.google.com/group/diagrams-discuss">project mailing list</a>, and/or come hang out in the <code>#diagrams</code> IRC channel on freenode.org.</p>
<ul>
<li><p>Cairo is the only well-supported backend at the moment, but you might create another backend or <a href="http://code.google.com/p/diagrams/wiki/BackendProjects">contribute to an existing project</a>.</p></li>
<li><p>The standard library is in need of additional features. Visit the <a href="http://code.google.com/p/diagrams/">Google Code site</a> for a list of open tickets.</p></li>
<li><p>Create a higher-level module built on top of the diagrams framework (e.g. tree or graph layout, generating Turing machine configuration diagrams, Penrose tilings ... your imagination is the only limit!) and submit it for inclusion in a special diagrams-contrib package which will be created for such higher-level user-contributed modules.</p></li>
<li><p>Use diagrams to create some cool graphics and submit them for inclusion in a gallery of examples (to be created soon).</p></li>
<li><p>Start your own project built on top of diagrams and let us know how it goes!</p></li>
<li><p>Last but certainly not least, just try it out for your pet graphics generation needs and contribute your bug reports and feature requests.</p></li>
</ul>
<p>Happy diagramming!</p>
<p>Brought to you by the diagrams team:</p>
<ul>
<li>Brent Yorgey</li>
<li>Ryan Yates</li>
</ul>
<p>with contributions from:</p>
<ul>
<li>Sam Griffin</li>
<li>Vilhelm Sjöberg</li>
<li>Luite Stegeman</li>
<li>Kanchalai Suveepattananont</li>
<li>Scott Walck</li>
</ul>


