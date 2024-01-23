---
title: 'Creating documents with embedded diagrams'
published: 2012-08-28T17:27:40Z
categories: haskell,projects,writing
tags: announcement,BlogLiterately,build,diagrams,document,dynamic,embedding,hint,LaTeX,Markdown
---

<p>If you read my <a href="http://byorgey.wordpress.com/2012/08/24/unordered-tuples-and-type-algebra/">recent post about type algebra</a>, you may have wondered how I got all those nice images in there. Surely creating the images and then inserting them into the post by hand would be rather tedious! Indeed, it would be, but that's not what I did. I'm quite pleased to announce the release of several tools for making this sort of thing not only possible but even convenient.</p>
<p>Behind it all is the recently released <a href="http://hackage.haskell.org/package/diagrams%2Dbuilder">diagrams-builder package</a>. Diagrams backends such as <a href="http://hackage.haskell.org/package/diagrams%2Dcairo">diagrams-cairo</a> give you a static way to render diagrams. <code>diagrams-builder</code> makes the process dynamic: it can take arbitrary snippets of Haskell code, merge them intelligently, and call out to <a href="http://hackage.haskell.org/package/hint">hint</a> to render a diagram represented by some Haskell expression.</p>
<p>As a specific application of <code>diagrams-builder</code>, I've released <a href="http://hackage.haskell.org/package/BlogLiterately%2Ddiagrams">BlogLiterately-diagrams</a>, a diagrams plugin for <a href="http://hackage.haskell.org/package/BlogLiterately">BlogLiterately</a>. This is what I used to produce the type algebra post. The entire post was written in a single Markdown document, with inline diagrams code in specially marked code blocks. <code>BlogLiterately-diagrams</code> handles compiling those code blocks and replacing them with the generated images; <code>BlogLiterately</code> automatically uploads the images to the server. For example, including</p>
<pre><code>```{.dia width='200'}
sq = square 1

foo 0 = sq
foo n = ((foo' ||| sq') === (sq' ||| foo')) # centerXY # scale 0.5
  where 
    foo'   = foo (n-1)
    sq'    = sq # fc (colors !! n)
    colors = [black, red, orange, yellow, green, blue]

dia = foo 5 # lw 0
```</code></pre>
<p>in the middle of a post results in</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/4ad96cdf96e31202a1851539a5c1c7e0.png" /><p class="caption"></p>
</div>
</div>
<p>being included in the generated HTML.</p>
<p>Another exciting thing to mention is the LaTeX package <code>diagrams-latex.sty</code>, included in the <code>diagrams-builder</code> distribution. It lets you embed diagrams in LaTeX documents in much the same way that <code>BlogLiterately-diagrams</code> lets you embed diagrams in blog posts. Just stick diagrams code between <code>\begin{diagram}</code> and <code>\end{diagram}</code> and compile the document with <code>pdflatex --enable-write18</code>. It probably needs more work to smooth out some rough edges, but it's quite usable as it is---in fact, I'm currently using it to create the slides for my Haskell Symposium presentation in a few weeks.</p>
<p>Just to give a little perspective, this is essentially why I started building diagrams, over four years ago now---I wanted to produce some illustrations for a blog post, but was unsatisfied with the existing tools I found. With these tools, I can finally say that I've fully achieved my vision of four years ago---though don't worry, my vision has grown much larger in the meantime!</p>

