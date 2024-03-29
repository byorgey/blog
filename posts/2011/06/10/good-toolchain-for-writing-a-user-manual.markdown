---
katex: true
title: 'Good toolchain for writing a user manual?'
published: 2011-06-10T13:00:58Z
categories: projects,writing
tags: 
---

<p>It's time to start writing a user manual for <a href="http://projects.haskell.org/diagrams">diagrams</a>. Haddock documentation is great when you have only, say, forgotten the type of the <code>frobnitz</code> function. However, it is woefully inadequate when you are just trying to figure out how to wibble your fromps (it turns out, of course, that <code>frobnitz</code> is quite general and can be used for, among other things, wibbling fromps; but how would you know to look in the <code>Diagrams.Frobnostication</code> module in the first place?).</p>
<p>So I'm looking for tools I can use to help write and publish the manual. For something short like a <a href="http://projects.haskell.org/diagrams/tutorial.html">tutorial</a>, <a href="http://johnmacfarlane.net/pandoc/">pandoc</a> works like a charm, but the user manual is going to be a much larger and more complex beast. Here are my requirements. I want to:</p>
<ul>
<li>write in Markdown (or some similar sort of markup language)</li>
<li>have the ability to generate not just a single HTML file, but multiple ones with appropriate next/previous links, a table of contents, and so on</li>
<li>include Haskell code and generate a syntax-highlighted version in the output; the source (or something easily generated from the source) should also be an actual, valid literate Haskell file that people can load in <code>ghci</code></li>
<li>have good support for internal hyperlinks, so different parts of the manual can easily link to one another.</li>
</ul>
<p>And some &quot;nice-to-have&quot; features:</p>
<ul>
<li>I'd like easy hooks into the build process, so I can (for example) replace certain snippets of Haskell code with links to the images generated by the code.</li>
<li>I'd like to be able to incorporate <a href="http://hackage.haskell.org/package/doc%2Dreview"><code>doc-review</code></a>, or something like it, so people can comment on each paragraph in the manual.</li>
<li>I want to mark some regions as &quot;collapsible&quot; so they will initially be displayed as just a &quot;placeholder&quot; of some sort which can be expanded and collapsed by clicking. These could include things like extra technical information which many readers will not care about, extra explanation for users unfamiliar with Haskell, and so on.</li>
</ul>
<p>So -- any ideas? Perhaps I really want the <a href="http://www.yesodweb.com/blog/2011/5/introducing%2Dyesod%2Dwiki">system used for the Yesod wiki</a>, but I'm assuming it would not be cheap. The most viable thing I have considered so far is using pandoc to generate docbook, and from there generating chunked HTML. This is nice in some ways but not ideal: pandoc actually can't do syntax highlighting when writing anything other than HTML; I don't know how I would include collapsible sections; and you can only use two heading levels if you want valid literate Haskell.</p>
<p>It's quite likely that there's no single thing that will do exactly what I want, and I'm quite willing to do some "glue" work to put some pieces together.  But I'd like to avoid as much wheel-reinvention as possible.  Any ideas?</p>


