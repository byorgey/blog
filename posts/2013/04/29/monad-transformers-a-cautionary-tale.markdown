---
katex: true
title: 'Monad transformers: a cautionary tale'
published: 2013-04-29T21:03:48Z
categories: haskell
tags: commutative,monad,random,transformer
---

<p>When writing the code in <a href="https://byorgey.github.io/blog/posts/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2.html">my previous post</a>, I wanted to have a monad which combined the ability to generate random numbers with the ability to fail. Naturally, I decided to use <code>RandT Maybe</code>. But when I tried to write a failing computation of this type, I got a type error:</p>
<pre><code>    No instance for (MonadPlus (RandT StdGen Maybe))
      arising from a use of `mzero'</code></pre>
<p>It seems that no one ever bothered to add a <code>MonadPlus</code> instance for <code>RandT</code>. Well, that’s easy to fix. Since <code>RandT</code> is just a newtype wrapper around <code>StateT</code> we can even derive a <code>MonadPlus</code> instance automatically using <code>-XGeneralizedNewtypeDeriving</code>. So I modified the <code>MonadRandom</code> package, and everything worked great.</p>
<p>…That is, everything worked great until I started to get some strange behavior—sometimes computations would hang when I expected them to complete quickly. I finally was able to boil it down to the following minimal example. <code>foo</code> succeeds or fails with equal probability; <code>bar</code> reruns <code>foo</code> until it succeeds.</p>
<pre><code><span>foo</span> <span style="color:red;">::</span> <span>RandT</span> <span>StdGen</span> <span>Maybe</span> <span>()</span>
<span>foo</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
  <span>r</span> <span style="color:red;">&lt;-</span> <span>getRandomR</span> <span style="color:red;">(</span><span class="hs-num">0</span><span style="color:red;">,</span><span class="hs-num">1</span><span style="color:red;">)</span>
  <span style="color:blue;font-weight:bold;">if</span> <span>r</span> <span>&lt;</span> <span class="hs-num">1</span><span>/</span><span class="hs-num">2</span> <span style="color:blue;font-weight:bold;">then</span> <span>return</span> <span>()</span> <span style="color:blue;font-weight:bold;">else</span> <span>mzero</span>

<span>bar</span> <span style="color:red;">::</span> <span>RandT</span> <span>StdGen</span> <span>Maybe</span> <span>()</span>
<span>bar</span> <span style="color:red;">=</span> <span>foo</span> <span>`mplus`</span> <span>bar</span></code></pre>
<p>Seems straightforward, right? <code>bar</code> should always succeed pretty much instantly, since there’s only a $1/2^n$ chance that it will have to call <code>foo</code> $n$ times.</p>
<p>However, this is not what happens: some of the time <code>bar</code> returns instantly as expected, and some of the time it hangs in what seems like an infinite loop! What gives?</p>
<p>Have you figured it out yet? (If you like these sorts of puzzles you might want to stop and see if you can figure out what was going on.) The problem is that the <code>mplus</code> operation for <code>RandT StdGen Maybe</code> runs both of its arguments with the same random seed! In other words, when a computation fails the generator state gets thrown away. And if we think about how monad transformers work this is actually not surprising. We have the following isomorphisms:</p>
<pre><code>   RandT StdGen Maybe ()
== StateT StdGen Maybe ()
== StdGen -&gt; Maybe ((), StdGen)</code></pre>
<p>So when a computation fails you just get <code>Nothing</code>—in particular you <em>don’t</em> get to see what the new <code>StdGen</code> value would have been, so you can’t (say) pass it along to the second argument of <code>mplus</code>. The upshot is that <code>bar</code> succeeds if the first call to <code>foo</code> happens to succeed; otherwise it simply keeps calling <code>foo</code> with the exact same seed and <code>foo</code> keeps failing every time.</p>
<p>The general principle here is that “the effects of inner monad transformers take precedence over the effects of outer transformers”—in this case the failure effect of the inner <code>Maybe</code> takes precedence and causes the random generator state to be lost.</p>
<p>So what I really wanted was <code>MaybeT (Rand StdGen)</code>, which—after adding a <code>MonadRandom</code> instance for <code>MaybeT</code>, now released as <a href="http://hackage.haskell.org/package/MonadRandom"><code>MonadRandom</code>-0.1.9</a>—works perfectly.</p>
<p>The moral of the story: monad transformers aren’t (in general) commutative! Think carefully about what order you want. (I actually <a href="https://byorgey.github.io/blog/posts/2011/02/24/enumerating-linear-inhabitants.html">wrote about this once before</a>; you’d think I would have learned my lesson.)</p>

