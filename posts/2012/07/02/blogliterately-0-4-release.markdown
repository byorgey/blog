---
title: BlogLiterately 0.4 release
published: 2012-07-02T17:43:54Z
categories: haskell,writing
tags: blogging,BlogLiterately,ghci,LaTeX
---

<p>I have just released version 0.4 of <a href="http://hackage.haskell.org/package/BlogLiterately"><code>BlogLiterately</code></a>, a tool for authoring and uploading blog posts (especially Haskelly ones). Rob Greayer authored the first few versions of this useful tool, for which I'm very thankful. However, he doesn't have the time or interest to maintain it anymore, and I had a few ideas for extending and improving it, so I offered to take over as the maintainer.</p>
<p>The <a href="http://byorgey.wordpress.com/blogliterately/">full (dog-fooded) documentation can be found here</a>. This blog post is just to announce the release and show off a few capabilities.</p>
<p>Posts are written in <a href="http://daringfireball.net/projects/markdown/">Markdown</a> format (as recognized by <a href="http://johnmacfarlane.net/pandoc/">pandoc</a>), and <code>BlogLiterately</code> handles converting them to HTML and uploading them to any blog that supports the <a href="http://www.xmlrpc.com/metaWeblogApi">MetaWeblog API</a> (such as WordPress). Haskell code can be syntax highlighted using <a href="http://www.cs.york.ac.uk/fp/darcs/hscolour/">hscolour</a> (and any code can be syntax highlighted using <a href="http://johnmacfarlane.net/highlighting-kate/">highlighting-kate</a>):</p>
<pre><code><span>&gt;</span> <span style="color:green;">-- An awesome Haskell function</span>
<span>&gt;</span> <span>fib</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span>
<span>&gt;</span> <span>fib</span> <span class="hs-num">0</span> <span style="color:red;">=</span> <span class="hs-num">0</span>
<span>&gt;</span> <span>fib</span> <span class="hs-num">1</span> <span style="color:red;">=</span> <span class="hs-num">1</span>
<span>&gt;</span> <span>fib</span> <span>n</span> <span style="color:red;">=</span> <span>fib</span> <span style="color:red;">(</span><span>n</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span> <span>+</span> <span>fib</span> <span style="color:red;">(</span><span>n</span><span style="color:green;">-</span><span class="hs-num">2</span><span style="color:red;">)</span>
</code></pre>
<p>Special support for WordPress LaTeX is built in: $latex \pi^2 / 6$. <code>ghci</code> sessions can be automatically generated from a list of inputs:</p>
<pre><code><span style="color:gray;">ghci&gt;</span> fib 20  
  6765

<span style="color:gray;">ghci&gt;</span> [1..10]  
  [1,2,3,4,5,6,7,8,9,10]
</code></pre>
<p>The one major planned feature that is still missing is uploading of embedded images. Sadly, this feature ran into a major roadblock in the form of inexplicably closed HTTP connections (can you help <a href="http://stackoverflow.com/questions/11277788/errorclosed-exception-from-network-http-simplehttp-trying-to-upload-images-vi">answer my StackOverflow question</a>?). Ultimately my goal is to have completely automated support for writing blog posts with inline <a href="http://projects.haskell.org/diagrams/">diagrams</a> code.</p>
<p>Enjoy!</p>

