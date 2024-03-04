---
katex: true
title: 'foldr is made of monoids'
published: 2012-11-06T02:01:12Z
categories: haskell,math
tags: foldr,free,lists,mconcat,monoids
---

<pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data</span><span>.</span><span>Monoid</span>
</code></pre>
<p>In his recent blog post <a href="http://web.jaguarpaw.co.uk/~tom/blog/posts/2012-11-04-what-is-foldr-made-of.html">What is foldr made of?</a>, Tom Ellis made the clever observation that <code>foldr</code> is equivalent in power to the combination of <code>map</code> and <code>compose</code>, where</p>
<pre><code><span>&gt;</span> <span>compose</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>a</span> <span style="color:red;">-&gt;</span> <span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>a</span>
<span>&gt;</span> <span>compose</span> <span>[]</span>     <span style="color:red;">=</span> <span>id</span>
<span>&gt;</span> <span>compose</span> <span style="color:red;">(</span><span>f</span><span>:</span><span>fs</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>f</span> <span>.</span> <span>compose</span> <span>fs</span>
</code></pre>
<p>We can then write</p>
<pre><code><span>&gt;</span> <span>foldr'</span> <span style="color:red;">::</span> <span style="color:red;">(</span><span>a</span> <span style="color:red;">-&gt;</span> <span>b</span> <span style="color:red;">-&gt;</span> <span>b</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>b</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>b</span>
<span>&gt;</span> <span>foldr'</span> <span>g</span> <span>z</span> <span>xs</span> <span style="color:red;">=</span> <span>compose</span> <span style="color:red;">(</span><span>map</span> <span>g</span> <span>xs</span><span style="color:red;">)</span> <span>z</span>
</code></pre>
<p>which (as Tom proves) is exactly equivalent to the usual <code>foldr</code>.</p>
<p>This is a really neat idea which I don’t remember ever seeing before. But now that I have, I realized that I could expand on it a bit—<code>compose</code> doesn’t come out of thin air quite as much as it might first appear.</p>
<p>Consider the standard function <code>mconcat</code> (from <code>Data.Monoid</code>), which combines a list of values from some <code>Monoid</code>:</p>
<pre><code><span>mconcat</span> <span style="color:red;">::</span> <span>Monoid</span> <span>m</span> <span style="color:red;">=&gt;</span> <span style="color:red;">[</span><span>m</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>m</span>
<span>mconcat</span> <span>[]</span>     <span style="color:red;">=</span> <span>mempty</span>
<span>mconcat</span> <span style="color:red;">(</span><span>m</span><span>:</span><span>ms</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>m</span> <span>`mappend`</span> <span>mconcat</span> <span>ms</span></code></pre>
<p>I claim that <em>in the presence of <code>map</code>, <code>compose</code> and <code>mconcat</code> are equivalent</em>. Why is that? First, it’s easy to see that <code>compose</code> can be implemented in terms of <code>mconcat</code>—we just instantiate it to the monoid of endofunctions (where <code>mempty = id</code> and <code>mappend = (.)</code>), with a little bit of syntactic noise due to the need to convert in and out of the <code>Endo</code> newtype:</p>
<pre><code><span>&gt;</span> <span>compose'</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>a</span> <span style="color:red;">-&gt;</span> <span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>a</span>
<span>&gt;</span> <span>compose'</span> <span style="color:red;">=</span> <span>appEndo</span> <span>.</span> <span>mconcat</span> <span>.</span> <span>map</span> <span>Endo</span>
</code></pre>
<p>Proving that <code>compose'</code> is the same as <code>compose</code> is not hard; I leave it to you as an exercise.</p>
<p>Implementing <code>mconcat</code> in terms of <code>compose</code> is a bit more interesting:</p>
<pre><code><span>&gt;</span> <span>mconcat'</span> <span style="color:red;">::</span> <span>Monoid</span> <span>m</span> <span style="color:red;">=&gt;</span> <span style="color:red;">[</span><span>m</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>m</span>
<span>&gt;</span> <span>mconcat'</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>$</span><span>mempty</span><span style="color:red;">)</span> <span>.</span> <span>compose</span> <span>.</span> <span>map</span> <span>mappend</span>
</code></pre>
<p>The key idea is that we can turn any value from some monoidal type <code>m</code> into a function <code>m -&gt; m</code> by partially applying <code>mappend</code>; composing these functions then corresponds to combining the original values, and the final value can be recovered by applying the resulting function to <code>mempty</code>.<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup> That is,</p>
<p>$m_1 \diamond m_2 \diamond \dots \diamond m_n = ((m_1 \diamond) \circ (m_2 \diamond) \circ \dots \circ (m_n \diamond))\ \varepsilon$</p>
<p>(where I have used $\diamond$ and $\varepsilon$ to represent <code>mappend</code> and <code>mempty</code>, respectively). Written out this way, I hope you can see why the equality holds by thinking about what the composition on the right evaluates to (and remembering the right identity law, $x \diamond \varepsilon = x$).</p>
<p>So we can also say that <code>foldr</code> = <code>map</code> + <code>mconcat</code>! This gets at the idea that lists are <em>free</em> (or <em>initial</em>) monoids, which intuitively means that of all monoids, they “preserve the most information”—up to the monoid laws, combining lists preserves <em>all</em> the information about the original lists and how you have combined them. This also means that there is a canonical way to “convert” a list into any other <code>Monoid</code>: that is, given a mapping <code>f :: a -&gt; m</code>, there is a canonical way to take a list <code>[a]</code> and turn it into an <code>m</code>, namely, <code>mconcat . map f</code>.</p>
<p>Let’s make the connection to <code>foldr</code> even more explicit. First, let’s swap around the order of arguments and add some parentheses which aren’t strictly necessary:</p>
<pre><code><span>foldr''</span> <span style="color:red;">::</span> <span style="color:red;">(</span><span>a</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>b</span> <span style="color:red;">-&gt;</span> <span>b</span><span style="color:red;">)</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>b</span> <span style="color:red;">-&gt;</span> <span>b</span><span style="color:red;">)</span></code></pre>
<p>Hmm, <code>b -&gt; b</code>… why does this look so familiar? I claim we can actually pull a similar trick<sup><a href="#fn2" class="footnoteRef" id="fnref2">2</a></sup> as with <code>compose</code>/<code>mconcat</code>, and replace <code>b -&gt; b</code> with <code>Monoid m =&gt; m</code> to come up with a function equivalent to <code>foldr''</code> (and hence <code>foldr</code> as well):</p>
<pre><code>fold :: Monoid m =&gt; (a -&gt; m) -&gt; [a] -&gt; m</code></pre>
<p>Hmm, so how would this be implemented? Let’s see…</p>
<pre><code><span>&gt;</span> <span>fold</span> <span style="color:red;">::</span> <span>Monoid</span> <span>m</span> <span style="color:red;">=&gt;</span> <span style="color:red;">(</span><span>a</span> <span style="color:red;">-&gt;</span> <span>m</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>m</span>
<span>&gt;</span> <span>fold</span> <span>f</span> <span style="color:red;">=</span> <span>mconcat</span> <span>.</span> <span>map</span> <span>f</span>
</code></pre>
<p>So actually, <code>fold</code> itself (and hence, equivalently, <code>foldr</code>) is the canonical mapping from a list down to any monoid which I mentioned earlier! And here we can see quite directly that <code>fold</code> is indeed equivalent to <code>mconcat</code> + <code>map</code>.</p>
<p>In summary: <code>foldr</code> is equivalent to <code>map</code> plus <code>compose</code>/<code>mappend</code>, because lists are free monoids.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>Incidentally, this amounts to saying that <code>mappend</code> itself is a <em>monoid homomorphism</em> (a structure-preserving map between monoids), and this is where <a href="http://www.haskell.org/haskellwiki/Difference_list">difference lists</a> come from. For more, see my <a href="http://www.cis.upenn.edu/~byorgey/publications.html">recent paper, <em>Monoids: Theme and Variations</em></a>.<a href="#fnref1">↩</a></p></li>
<li id="fn2"><p>I leave this as an exercise too.<a href="#fnref2">↩</a></p></li>
</ol>
</div>

