---
katex: true
title: 'Introducing diagrams-haddock'
published: 2013-03-24T00:24:18Z
categories: haskell,writing
tags: diagrams,documentation,Haddock,images
---

<p>I am quite pleased to announce the release of <a href="http://hackage.haskell.org/package/diagrams%2Dhaddock">diagrams-haddock</a>, a tool enabling you to <strong>easily include programmatically generated diagrams in your Haddock documentation</strong>. Why might you want to do this? “A picture is worth a thousand words”—in many instances a diagram or illustration can dramatically increase the comprehension of users reading your library’s documentation. The <a href="http://projects.haskell.org/diagrams"><code>diagrams</code> project</a> itself will be using this for documentation, beginning with the <a href="http://hackage.haskell.org/package/diagrams%2Dcontrib"><code>diagrams-contrib</code> package</a> (for example, check out the <a href="http://hackage.haskell.org/packages/archive/diagrams-contrib/0.6.1/doc/html/Diagrams-TwoD-Path-IteratedSubset.html">documentation for Diagrams.TwoD.Path.IteratedSubset</a>). But inline images can benefit the documentation of just about any library.</p>
<p>Before jumping into a more detailed example, here are the main selling points of <code>diagrams-haddock</code>:</p>
<ol style="list-style-type:decimal;">
<li><p>You get to create arbitrary images to enhance your documentation, using the powerful <a href="http://projects.haskell.org/diagrams"><code>diagrams</code> framework</a>.</p></li>
<li><p>The code for your images goes <em>right into your source files themselves</em>, alongside the documentation—there is no need to maintain a bunch of auxiliary files, or (heaven forbid) multiple versions of your source files.</p></li>
<li><p>Images are regenerated when, and <em>only</em> when, their definition changes—so you can include many diagrams in your documentation without having to recompile all of them every time you make a change to just one.</p></li>
<li><p>You have to do a little bit of work to integrate the generated images into your Cabal package, but it’s relatively simple and you only have to do it once per package. No one else needs to have <code>diagrams-haddock</code> installed in order to build your documentation with the images (this includes Hackage).</p></li>
</ol>
<p>So, how does it work? (For full details, consult the <a href="https://github.com/diagrams/diagrams-haddock/blob/master/README.md"><code>diagrams-haddock</code> documentation</a>.) Suppose we have some Haddock documentation that looks like this:</p>
<pre><code><span style="color:green;">-- | The foozle function takes a widget and turns it into an</span>
<span style="color:green;">--   infinite list of widgets which alternate between red and</span>
<span style="color:green;">--   yellow.</span>
<span style="color:green;">--</span>
<span>foozle</span> <span style="color:red;">::</span> <span>Widget</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Widget</span><span style="color:red;">]</span>
<span>foozle</span> <span style="color:red;">=</span> <span>...</span></code></pre>
<p>It would be really nice to illustrate this with a picture, don’t you think? First, we insert an image placeholder like so:</p>
<pre><code><span style="color:green;">-- | The foozle function takes a widget and turns it into an</span>
<span style="color:green;">--   infinite list of widgets which alternate between red and</span>
<span style="color:green;">--   yellow.</span>
<span style="color:green;">--</span>
<span style="color:green;">--   &lt;&lt;dummy#diagram=foozleDia&amp;width=300&gt;&gt;</span>
<span style="color:green;">--</span>
<span>foozle</span> <span style="color:red;">::</span> <span>Widget</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Widget</span><span style="color:red;">]</span>
<span>foozle</span> <span style="color:red;">=</span> <span>...</span></code></pre>
<p>It doesn’t matter what we put in place of <code>dummy</code>; <code>diagrams-haddock</code> is going to shortly replace it anyway. The stuff following the <code>#</code> is a list of parameters to <code>diagrams-haddock</code>: we tell it to insert here an image built from the diagram called <code>foozleDia</code>, and that it should have a width of 300 pixels.</p>
<p>Now we just have to give a definition for <code>foozleDia</code>, which we do simply by creating a code block (set off with bird tracks) in a comment:</p>
<pre><code><span style="color:green;">-- | The foozle function takes a widget and turns it into an</span>
<span style="color:green;">--   infinite list of widgets which alternate between red and</span>
<span style="color:green;">--   yellow.</span>
<span style="color:green;">--</span>
<span style="color:green;">--   &lt;&lt;dummy#diagram=foozleDia&amp;width=300&gt;&gt;</span>
<span style="color:green;">--</span>
<span>foozle</span> <span style="color:red;">::</span> <span>Widget</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Widget</span><span style="color:red;">]</span>
<span>foozle</span> <span style="color:red;">=</span> <span>...</span>

<span style="color:green;">-- &gt; widget =</span>
<span style="color:green;">-- &gt;   (  stroke (circle 1.25 &lt;&gt; circle 0.75 # reversePath)</span>
<span style="color:green;">-- &gt;   &lt;&gt; mconcat (iterateN 10 (rotateBy (1/10)) (square 0.5 # translateX 1.3))</span>
<span style="color:green;">-- &gt;   )</span>
<span style="color:green;">-- &gt;   # lw 0</span>
<span style="color:green;">-- &gt;</span>
<span style="color:green;">-- &gt; foozleDia =</span>
<span style="color:green;">-- &gt;   hcat' with {sep = 2}</span>
<span style="color:green;">-- &gt;   [ widget # fc black</span>
<span style="color:green;">-- &gt;   , hrule 4 # alignR &lt;&gt; triangle 1 # rotateBy (-1/4) # fc black</span>
<span style="color:green;">-- &gt;   , hcat' with {sep = 0.5} (zipWith fc (cycle [red, yellow]) (replicate 6 widget))</span>
<span style="color:green;">-- &gt;   ]</span></code></pre>
<p>Note that this definition for <code>foozleDia</code> isn’t in a Haddock comment, so it won’t be typeset in the Haddock output. (However, if you want users reading your documentation to see the code used to generate the pictures—as, <em>e.g.</em>, we often do in the documentation for <code>diagrams</code> itself—it’s as simple as sticking the definitions in a Haddock comment.) It also doesn’t have to go right after the definition of <code>foozle</code>—for example, we could stick it all the way at the end of the source file if we didn’t want it cluttering up the code.</p>
<p>Now we simply run <code>diagrams-haddock</code> on our file (or on the whole <code>Cabal</code> project), and it will generate an appropriate SVG image and replace <code>&lt;&lt;dummy#...&gt;&gt;</code> with something like <code>&lt;&lt;diagrams/foozleDia.svg#...&gt;&gt;</code>. The Haddock documentation now displays something like</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2013/03/97bc1304d6782598753944eab0ba9252.png" /></p>
</div>
<p>after the documentation for <code>foozle</code>. Hooray! Note that <code>diagrams-haddock</code> only replaces the stuff before the <code>#</code> (the clever bit is that browsers will ignore everything after the <code>#</code>). Running <code>diagrams-haddock</code> again at this point will do nothing. If we change the definition of <code>foozleDia</code> and then rerun <code>diagrams-haddock</code>, it will regenerate the image.</p>
<p>Okay, but how will others (or, for that matter, Hackage) be able to see the diagram for <code>foozle</code> when they build the documentation, without needing <code>diagrams-haddock</code> themselves? It’s actually fairly straightforward—we simply include the generated images in the source tarball, and tell <code>cabal</code> to copy the images in alongside the documentation when it is built, using either a custom <code>Setup.hs</code>, or (once it is released and sufficiently ubiquitous) the new <code>extra-html-files:</code> field in the <code>.cabal</code> file. The <a href="https://github.com/diagrams/diagrams-haddock/blob/master/README.md"><code>diagrams-haddock</code> documentation</a> has full details with step-by-step instructions.</p>
<p>I hope this silly example has piqued your interest; again, for full details please consult the <a href="https://github.com/diagrams/diagrams-haddock/blob/master/README.md"><code>diagrams-haddock</code> documentation</a>. Now go forth and illustrate your documentation!</p>

