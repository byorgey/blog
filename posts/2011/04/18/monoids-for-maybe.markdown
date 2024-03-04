---
katex: true
title: 'Monoids for Maybe'
published: 2011-04-18T14:46:20Z
categories: haskell
tags: applicative,instances,Maybe,monoid
---

<p>The other day I had two lists of monoidal values that I wanted to combine in a certain way, and I realized it was an instance of this more general pattern:</p><pre><code><span>&gt;</span> <span style="color:green;">{-# LANGUAGE GeneralizedNewtypeDeriving #-}</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data</span><span>.</span><span>Monoid</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Control</span><span>.</span><span>Applicative</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:red;">(</span><span>&lt;&gt;</span><span style="color:red;">)</span> <span style="color:red;">::</span> <span>Monoid</span> <span>m</span> <span style="color:red;">=&gt;</span> <span>m</span> <span style="color:red;">-&gt;</span> <span>m</span> <span style="color:red;">-&gt;</span> <span>m</span>
<span>&gt;</span> <span style="color:red;">(</span><span>&lt;&gt;</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>mappend</span>   <span style="color:green;">-- I can't stand writing `mappend`</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">newtype</span> <span>AM</span> <span>f</span> <span>m</span> <span style="color:red;">=</span> <span>AM</span> <span style="color:red;">{</span> <span>unAM</span> <span style="color:red;">::</span> <span>f</span> <span>m</span> <span style="color:red;">}</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Functor</span><span style="color:red;">,</span> <span>Applicative</span><span style="color:red;">,</span> <span>Show</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span style="color:red;">(</span><span>Applicative</span> <span>f</span><span style="color:red;">,</span> <span>Monoid</span> <span>m</span><span style="color:red;">)</span> <span style="color:red;">=&gt;</span> <span>Monoid</span> <span style="color:red;">(</span><span>AM</span> <span>f</span> <span>m</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>mempty</span>        <span style="color:red;">=</span> <span>pure</span> <span>mempty</span>
<span>&gt;</span>   <span>mappend</span> <span>f1</span> <span>f2</span> <span style="color:red;">=</span> <span>mappend</span> <span>&lt;$&gt;</span> <span>f1</span> <span>&lt;*&gt;</span> <span>f2</span>
</code></pre><p>It's not too hard (although a bit fiddly) to show that <code>AM f m</code> satisfies the monoid laws, given that <code>f</code> and <code>m</code> satisfy the applicative functor and monoid laws respectively.</p><p>The basic idea here is that the <code>mappend</code> operation for <code>AM f m</code> is just the <code>mappend</code> operation for <code>m</code>, but applied &quot;idiomatically&quot; in the <code>f</code> context. For example, when <code>f = []</code>, this combines two lists of monoidal values by combining all possible pairs:</p><pre><code>*Main&gt; map getProduct . unAM $ (AM (map Product [1,2,3]) 
                                &lt;&gt; AM (map Product [1,10,100]))
[1,10,100,2,20,200,3,30,300]</code></pre><p>In the <code>#haskell</code> IRC channel someone pointed out to me that <code>Data.Monoid</code> has an <code>instance Monoid m =&gt; Monoid (e -&gt; m)</code> which is just a special case of this pattern:</p><pre><code>*Main&gt; :m +Data.Ord
*Main Data.Ord&gt; map ((unAM $ AM (comparing length) 
                             &lt;&gt; AM compare) "foo") 
                    ["ba", "bar", "barr"]
[GT,GT,LT]
*Main Data.Ord&gt; map ((comparing length &lt;&gt; compare) "foo") 
                    ["ba", "bar", "barr"]
[GT,GT,LT]</code></pre><p>It was also mentioned that the monoid instance for <code>Maybe</code> is also a special case of this pattern:</p><pre><code>*Main&gt; AM (Just (Sum 3)) &lt;&gt; AM Nothing
AM {unAM = Nothing}
*Main&gt; Just (Sum 3) &lt;&gt; Nothing
Just (Sum {getSum = 3})</code></pre><p>Wait, hold on, what?! It turns out that the default <code>Monoid</code> instance for <code>Maybe</code> is <em>not</em> an instance of this pattern after all! I previously thought there were <em>three</em> different ways of declaring a <code>Monoid</code> instance for <code>Maybe</code>; I now know that there are (at least) <em>four</em>.</p><ul><li>The default instance defined in <code>Data.Monoid</code> uses <code>Nothing</code> as the identity element, so <code>Nothing</code> represents &quot;no information&quot;. It requires a <code>Monoid</code> constraint on the type wrapped by <code>Maybe</code>, although <code>Monoid</code> is slightly too strong, since the type's own identity element is effectively ignored. In fact, the <code>Data.Monoid</code> documentation states<blockquote>
    Lift a semigroup into <code>Maybe</code> forming a <code>Monoid</code> according to <a href="http://en.wikipedia.org/wiki/Monoid">http://en.wikipedia.org/wiki/Monoid</a>: &quot;Any semigroup $S$ may be turned into a monoid simply by adjoining an element $e$ not in $S$ and defining $e*e = e$ and $e*s = s = s*e$ for all $s \in S$.&quot; Since there is no <code>Semigroup</code> type class providing just <code>mappend</code>, we use <code>Monoid</code> instead.</blockquote>
    <p>(Actually, there is (now) <a href="http://hackage.haskell.org/packages/archive/semigroups/0.3.4.1/doc/html/Data-Semigroup.html#t:Semigroup">a <code>Semigroup</code> type class</a>...)</p><pre><code>*Main&gt; mconcat [Just (Sum 3), Nothing]
Just (Sum {getSum = 3})
*Main&gt; mconcat [Just (Sum 3), Nothing, Just (Sum 4), Nothing]
Just (Sum {getSum = 7})
</code></pre></li><li><p>The <code>First</code> newtype wrapper in <code>Data.Monoid</code> just takes the first non-<code>Nothing</code> occurrence:</p><pre><code>*Main&gt; mconcat . map First $ [Nothing, Just 3, Nothing, Just 4]
First {getFirst = Just 3}
</code></pre><p>This is actually the same as the <code>MonadPlus</code> instance for <code>Maybe</code>, where <code>mplus</code> is used to choose the first non-failing computation:</p><pre><code>*Main Control.Monad&gt; Nothing `mplus` 
                     Just 3 `mplus` 
                     Nothing `mplus` 
                     Just 4
Just 3
</code></pre></li><li><p>The <code>Last</code> newtype wrapper is the dual of <code>First</code>, taking the <em>last</em> non-<code>Nothing</code> occurrence:</p><pre><code>*Main&gt; mconcat . map Last $ [Nothing, Just 3, Nothing, Just 4]
Last {getLast = Just 4}
</code></pre></li><li><p>The <code>Monoid</code> instance following the <code>Applicative</code> structure of <code>Maybe</code>, however, is distinct from all of these. It combines values wrapped by <code>Just</code> according to their own <code>Monoid</code> instance, but if any occurrences of <code>Nothing</code> are encountered, the result is also <code>Nothing</code>. That is, it corresponds to combining monoidal values in the presence of possible failure, that is, applying <code>mappend</code> idiomatically within the applicative context.</p><pre><code>*Main&gt; mconcat [AM (Just (Sum 3)), AM (Just (Sum 4))]
AM {unAM = Just (Sum {getSum = 7})}
*Main&gt; mconcat [AM (Just (Sum 3)), AM (Just (Sum 4)), AM Nothing]
AM {unAM = Nothing}
</code></pre><p>As far as I know, this instance is nowhere to be found in the standard libraries. Perhaps a wrapper like <code>AM</code> should be added to <code>Control.Applicative</code>?</p></li></ul>

