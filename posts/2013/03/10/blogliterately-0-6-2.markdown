---
title: 'BlogLiterately 0.6'
published: 2013-03-10T17:38:15Z
categories: haskell,writing
tags: BlogLiterately,options,release
---

<p>I’m very proud to announce the release of <a href="http://hackage.haskell.org/package/BlogLiterately-0.6"><code>BlogLiterately</code> version 0.6</a>, a tool for formatting and uploading blog posts, including syntax highlighting, generation of <code>ghci</code> sessions, LaTeX support, automatic image uploading, and more.</p>
<p><strong>tl;dr</strong>: Instead of cumbersomely specifying all options on the command-line, you can now specify options using a combination of “profiles” (<em>e.g.</em> for common sets of options such as blog URL and password) and options embedded within the <code>.markdown</code> or <code>.lhs</code> documents themselves (<em>e.g.</em> for post-specific options like title, tags, and categories).</p>
<p>There are a few other changes and improvements as well. For more information, see the <a href="http://byorgey.wordpress.com/blogliterately/">documentation</a> or keep reading below!</p>
<h2 id="specifying-options">Specifying options</h2>
<p>With previous releases, uploading a post usually went something like this:</p>
<pre><code>BlogLiterately MyPost.md --blog "http://my.blog.url/xmlrpc.php \
  --user me --password 1234567 --postid 9999 --title "My awesome post" \
  --tag tag1 --tag tag2 --tag tag3 --category Stuff \
  --category OtherStuff --ghci --wplatex</code></pre>
<p>which is incredibly tedious and error-prone. Now we do things the Right Way (tm). First, you can create one or more <em>profiles</em>, specifying a common set of options that can be referred to by name. For example, you might have a profile for a particular blog, or a profile for a particular type of post which always needs the same options. Suppose we put this in <code>$HOME/.BlogLiterately/foo.cfg</code> (or in something like <code>C:/Documents And Settings/user/Application Data/BlogLiterately/foo.cfg</code> on Windows):</p>
<pre><code>blog        = http://my.blog.url/xmlrpc.php
user        = me
password    = 1234567
wplatex     = true</code></pre>
<p>Now the previous command line is reduced to</p>
<pre><code>BlogLiterately MyPost.md -P foo --postid 9999 --title "My awesome post" \
  --tag tag1 --tag tag2 --tag tag3 --category Stuff \
  --category OtherStuff --ghci</code></pre>
<p>which is already a big improvement! But it doesn’t stop there. The title, tags, categories, and other such things are really inherent to the post itself; there’s no reason they should go on the command line. So, we add this indented block somewhere in <code>MyPost.md</code> (probably near the top, though it doesn’t matter):</p>
<pre><code>    [BLOpts]
    profile    = foo
    postid     = 9999
    title      = "My awesome post"
    tags       = tag1, tag2, tag3
    categories = Stuff, OtherStuff
    ghci       = true</code></pre>
<p>And now we only have to write</p>
<pre><code>BlogLiterately MyPost.md</code></pre>
<p>with no options on the command line at all! Notice how we can even specify which profile to use in the <code>[BLOpts]</code> block. When we’re satisfied with the post we can publish it with</p>
<pre><code>BlogLiterately MyPost.md --publish</code></pre>
<h2 id="generating-html-only">Generating HTML only</h2>
<p>In the past, to get a “preview” version of the HTML output written to stdout, all you had to do was omit a <code>--blog</code> option. However, if you specify a profile with a <code>blog</code> field as in the above example, this is more problematic. For this reason, a new option <code>--html-only</code> has been added. When this option is specified, nothing will be uploaded, and the HTML output written to stdout.</p>
<h2 id="changes-to-transforms">Changes to Transforms</h2>
<p>In order to make the above features possible, the definition of <code>Transform</code> has changed. This only affects those users who have created their own custom transformations. The definition used to be</p>
<pre><code><span style="color:blue;font-weight:bold;">data</span> <span>Transform</span>
  <span style="color:red;">=</span> <span>Transform</span>
    <span style="color:red;">{</span> <span>getTransform</span> <span style="color:red;">::</span> <span>BlogLiterately</span> <span style="color:red;">-&gt;</span> <span>Kleisli</span> <span>IO</span> <span>Pandoc</span> <span>Pandoc</span>
    <span style="color:red;">,</span> <span>xfCond</span>       <span style="color:red;">::</span> <span>BlogLiterately</span> <span style="color:red;">-&gt;</span> <span>Bool</span>
    <span style="color:red;">}</span></code></pre>
<p>that is, a <code>Transform</code> was a transformation on <code>Pandoc</code> documents, parameterized by an options record and able to have effects in the <code>IO</code> monad. The definition is now</p>
<pre><code><span style="color:blue;font-weight:bold;">data</span> <span>Transform</span>
  <span style="color:red;">=</span> <span>Transform</span>
    <span style="color:red;">{</span> <span>getTransform</span> <span style="color:red;">::</span> <span>StateT</span> <span style="color:red;">(</span><span>BlogLiterately</span><span style="color:red;">,</span> <span>Pandoc</span><span style="color:red;">)</span> <span>IO</span> <span>()</span>
    <span style="color:red;">,</span> <span>xfCond</span>       <span style="color:red;">::</span> <span>BlogLiterately</span> <span style="color:red;">-&gt;</span> <span>Bool</span>
    <span style="color:red;">}</span></code></pre>
<p>meaning that a <code>Transform</code> is able to transform <em>both</em> a <code>Pandoc</code> document <em>and</em> the options record. This is crucial for being able to do things like embedding options within the document itself, because we don’t know all the options until we start processing the document! Also, I switched from using <code>Kleisli</code> arrows to using <code>StateT</code>, since I find it simpler to work with, especially now that multiple pieces of state are involved. For more information and help upgrading, see the <a href="http://hackage.haskell.org/packages/archive/BlogLiterately/latest/doc/html/Text-BlogLiterately-Transform.html">documentation for <code>Text.BlogLiterately.Transform</code></a>.</p>
<h2 id="move-to-github">Move to github</h2>
<p>The other change is that I have moved the <code>BlogLiterately</code> repository from darcshub to github. In general, for small personal projects and miscellaneous sorts of things I use <code>darcs</code> and <code>hub.darcs.net</code>; for larger projects where I want to raise the visibility and encourage contributions from other users, I use github. At some point <code>BlogLiterately</code> crossed the line.</p>
<h2 id="learning-more-and-contacting-me">Learning more, and contacting me</h2>
<p>For more information, see the <a href="http://byorgey.wordpress.com/blogliterately/">full documentation</a>. I’m always happy to receive comments, questions, feature requests, bug reports, and so on, via the <a href="https://github.com/byorgey/BlogLiterately/issues">bug tracker on github</a>, IRC (<code>byorgey</code> on freenode), or email (the same as my IRC nick, at gmail).</p>

