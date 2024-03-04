---
katex: true
title: 'Generating plane tilings with diagrams'
published: 2011-11-12T16:41:19Z
categories: haskell,math,projects
tags: diagrams,plane,tiling
---

I've finally set up a <a href="https://patch-tag.com/r/byorgey/diagrams-contrib/home">diagrams-contrib package</a> to serve as a home for user contributions to the <a href="http://projects.haskell.org/diagrams">diagrams project</a>---generation of specialized diagrams, fun or instructive examples, half-baked ideas, stuff which is not sufficiently polished or general to go in the <a href="http://hackage.haskell.org/package/diagrams-lib">diagrams-lib package</a> but is nonetheless worth sharing.

As the first "contribution" I put some <a href="https://patch-tag.com/r/byorgey/diagrams-contrib/snapshot/current/content/pretty/src/Diagrams/TwoD/Tilings.hs">code I wrote for fun</a> that generates tilings of the Euclidean plane by regular polygons.

<a href="http://byorgey.files.wordpress.com/2011/11/t3.png"><img src="http://byorgey.files.wordpress.com/2011/11/t3.png" alt="" title="t3" width="200" height="200" class="size-full wp-image-699" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t4.png"><img src="http://byorgey.files.wordpress.com/2011/11/t4.png" alt="" title="t4" width="200" height="200" class="size-full wp-image-700" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t33434.png"><img src="http://byorgey.files.wordpress.com/2011/11/t33434.png" alt="" title="t33434" width="200" height="200" class="size-full wp-image-710" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t3636.png"><img src="http://byorgey.files.wordpress.com/2011/11/t3636.png" alt="" title="t3636" width="200" height="200" class="size-full wp-image-704" /></a>

<p>So how does it work?  I'm sure there are more clever ways if you understand the mathematics better; but essentially it does a depth-first search along the edge graph, stopping when it reaches some user-defined limit, and drawing polygons and edges along the way.  This sounds quite simple on the face of it; but there are two nontrivial problems to be worked out:</p>

<ol>
	<li>How can we tell whether we've visited a given vertex before?</li>
	<li>How do we represent a tiling in a way that lets us easily traverse its edge graph?</li>
</ol>

<a href="http://byorgey.files.wordpress.com/2011/11/t33344.png"><img src="http://byorgey.files.wordpress.com/2011/11/t33344.png" alt="" title="t33344" width="200" height="200" class="size-full wp-image-709" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t488.png"><img src="http://byorgey.files.wordpress.com/2011/11/t488.png" alt="" title="t488" width="200" height="200" class="size-full wp-image-702" /></a>

The first question is really a question of <i>representation</i>: how do we represent vertices in such a way that we can decide their equality?  Representing them with a pair of floating point coordinates does not work: taking two different paths to a vertex will surely result in slightly different coordinates due to floating point error. Another idea is to represent vertices by the <i>path</i> taken to reach them, but now we have to deal with the thorny problem of deciding when two paths are equivalent.

But it turns out we can do something a bit more clever. The only regular polygons that can appear in plane tilings are triangles, squares, hexagons, octagons, and dodecagons.  If you remember your high school trigonometry, these all have "special" angles whose sines and cosines can be represented exactly using square roots.  It suffices to work in $\mathbb{Q}[\sqrt{2}, \sqrt{3}]$, that is, the ring of rational numbers adjoined with $\sqrt{2}$ and $\sqrt{3}$.  Put simply, we use quadruples of rational numbers $(a,b,c,d)$ which represent the real number $a + b\sqrt{2} + c\sqrt{3} + d\sqrt{6}$.  Now we can represent vertices <i>exactly</i>, so remembering which we've already visited is easy.

<a href="http://byorgey.files.wordpress.com/2011/11/t33336r.png"><img src="http://byorgey.files.wordpress.com/2011/11/t33336r.png" alt="" title="t33336R" width="200" height="200" class="alignnone size-full wp-image-708" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t33336l.png"><img src="http://byorgey.files.wordpress.com/2011/11/t33336l.png" alt="" title="t33336L" width="200" height="200" class="alignnone size-full wp-image-707" /></a>

The other question is how to represent tilings.  I chose to use this "zipper-like" representation:

<pre><code>data Tiling = Tiling [TilingPoly] (Int -&gt; Tiling)</code></pre>

Intuitively, a <code>Tiling</code> tells us what polygons surround the current vertex (ordered counterclockwise from the edge along which we entered the vertex), as well as what configurations we can reach by following edges out of the current vertex.  Thanks to laziness and knot-tying, we can easily define infinite tilings, such as

<pre><code>
t4 :: Tiling
t4 = Tiling (replicate 4 Square) (const t4)
</code></pre>

This is a particularly simple example, but the principle is the same.  You can <a href="https://patch-tag.com/r/byorgey/diagrams-contrib/snapshot/current/content/pretty/src/Diagrams/TwoD/Tilings.hs">look at the source</a> for more complex examples.

<a href="http://byorgey.files.wordpress.com/2011/11/t31212.png"><img src="http://byorgey.files.wordpress.com/2011/11/t31212.png" alt="" title="t31212" width="200" height="200" class="alignnone size-full wp-image-706" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t4612.png"><img src="http://byorgey.files.wordpress.com/2011/11/t4612.png" alt="" title="t4612" width="200" height="200" class="alignnone size-full wp-image-705" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t3464.png"><img src="http://byorgey.files.wordpress.com/2011/11/t3464.png" alt="" title="t3464" width="200" height="200" class="alignnone size-full wp-image-703" /></a><a href="http://byorgey.files.wordpress.com/2011/11/t6.png"><img src="http://byorgey.files.wordpress.com/2011/11/t6.png" alt="" title="t6" width="200" height="200" class="alignnone size-full wp-image-701" /></a>

Of course, this doesn't really show off the capabilities of <code>diagrams</code> much (you can draw regular polygons with any old graphics library), but it sure was fun!




