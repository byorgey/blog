---
title: BlogLiterately 0.5 release
published: 2012-07-07T18:06:31Z
categories: haskell,writing
tags: blogging,BlogLiterately,ghci,images,release,upload
---

<p>I have now released version 0.5 of <a href="http://hackage.haskell.org/package/BlogLiterately"><code>BlogLiterately</code></a>. (You can <a href="http://byorgey.wordpress.com/2012/07/02/blogliterately-0-4-release/">read about the 0.4 release here</a>.) This version does uploading of images! Here is proof:</p>

<div style='text-align:center;'>
<img src="http://byorgey.files.wordpress.com/2012/07/puppy-small.jpg" />
</div>

<p>d'awww.</p>
<p>(<a href="http://byorgey.wordpress.com/2012/07/07/new-haxr-release/">My previous post</a> explains the problem and solution with image uploads.)</p>
<p>It also allows you to specify <em>expected outputs</em> in a <code>ghci</code> session (a feature <a href="http://www.reddit.com/r/haskell/comments/vz5vn/blogliterately_04_brent_yorgeys_tool_for/c5936th">suggested by Dan Burton</a>). This block</p>
<pre><code>    [ghci]
    7+6
      13
    9+4
      12</code></pre>
<p>now produces</p>
<pre><code><span style="color:gray;">ghci&gt; </span>7+6
  13

<span style="color:gray;">ghci&gt; </span>9+4
  <span style="color:red;">13</span>
  <span style="color:blue;">12</span>
</code></pre>
<p>Outputs that match the expected output are shown normally; outputs that don't match the expected output are shown with the actual output in red and expected in blue. The idea is that this helps you catch errors in your code before uploading the post. (Of course, you don't have to specify expected outputs if you don't want to.)</p>
<p>Another new feature is that <code>BlogLiterately</code> will prompt you for your password if you don't specify it on the command line (another feature requested by Dan).</p>
<p>Finally, one of the coolest new features (in my opinion) is that the internals are now <a href="http://hackage.haskell.org/package/BlogLiterately">exposed as a library</a>, and in particular you can <a href="http://hackage.haskell.org/packages/archive/BlogLiterately/0.5/doc/html/Text-BlogLiterately-Run.html">easily add your own custom transformation passes</a> (of type <code>Pandoc -&gt; IO Pandoc</code>) to the existing ones. So, for example, you could do something particular with your own specially tagged blocks (like <code>[ghci]</code> blocks), or wrap images in some fancy HTML to produce frames and captions, or automatically turn certain things into links, or whatever you can dream up. If you come up with any transformations you think might be more generally useful, please send them to me so I can include them in future releases for others to use.</p>
<p>Happy blogging!</p>

