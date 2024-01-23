---
title: 'The generic-random library, part 1: simple generic Arbitrary instances'
published: 2016-09-20T22:27:21Z
categories: combinatorics,haskell
tags: Arbitrary,generic,QuickCheck,random
---

<p>(<strong>Note, 9/21/2017</strong>: The <code>generic-random</code> package has been changed and updated quite a bit since I wrote this post. For more up-to-date information, see <em>e.g.</em> the <a href="http://hackage.haskell.org/package/generic-random/docs/Generic-Random-Tutorial.html">tutorial</a> included with the package.)</p>
<p>In a <a href="https://byorgey.wordpress.com/2016/03/23/boltzmann-sampling-for-generic-arbitrary-instances/">previous post</a> I pointed out that we know all the theory to make nice, principled, practical random generators for recursive algebraic data types; someone just needed to step up and do the work. Well, <a href="https://www.eleves.ens.fr/home/xia/">Li-yao Xia</a> took up the challenge and produced a brilliant package, <a href="http://hackage.haskell.org/package/generic-random">generic-random</a>, available on Hackage right now for you to use!</p>
<p>However, although the package does include some Haddock documentation, it is probably difficult for someone with no experience or background in this area to navigate. So I thought it would be worth writing a few blog posts by way of a tutorial and introduction to the package.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:green;">{-# LANGUAGE GADTSyntax           #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveGeneric        #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE FlexibleContexts     #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE UndecidableInstances #-}</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>GHC</span><span>.</span><span>Generics</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Test</span><span>.</span><span>QuickCheck</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Generic</span><span>.</span><span>Random</span><span>.</span><span>Generic</span>
</code></pre>
<h2 id="the-problem">The problem</h2>
<p>First, a quick recap of the problem we are trying to solve: the obvious, naive way of generating random instances of some recursive algebraic data type often produces really terrible distributions. For example, one might generate really tiny structures most of the time and then occasionally generate a humongous one. For more background on the problem, see <a href="https://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/">this post</a> or <a href="https://byorgey.wordpress.com/2016/03/23/boltzmann-sampling-for-generic-arbitrary-instances/">this one</a>.</p>
<h2 id="a-first-example-generating-generic-arbitrary-instances">A first example: generating generic <code>Arbitrary</code> instances</h2>
<p>As a first example, consider the following algebraic data type:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Foo</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>Bar</span>  <span style="color:red;">::</span> <span>Char</span> <span style="color:red;">-&gt;</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>Foo</span>
<span>&gt;</span>   <span>Baz</span>  <span style="color:red;">::</span> <span>Bool</span> <span style="color:red;">-&gt;</span> <span>Bool</span> <span style="color:red;">-&gt;</span> <span>Foo</span>
<span>&gt;</span>   <span>Quux</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Woz</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Foo</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Generic</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Woz</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>Wiz</span> <span style="color:red;">::</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>Woz</span>
<span>&gt;</span>   <span>Waz</span> <span style="color:red;">::</span> <span>Bool</span> <span style="color:red;">-&gt;</span> <span>Woz</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Generic</span><span style="color:red;">)</span>
</code></pre>
<p>You have probably noticed by now that this is <em>not</em> recursive (well, except for the embedded lists). Patience! We’ll get to recursive ADTs in due time, but it turns out the library has some nice things to offer for non-recursive ADTs as well, and it makes for an easier introduction.</p>
<p>Now, suppose we wanted to use <a href="http://hackage.haskell.org/package/QuickCheck">QuickCheck</a> to test some properties of a function that takes a <code>Foo</code> as an argument. We can easily make our own instances of <code>Arbitrary</code> for <code>Foo</code> and <code>Woz</code>, like so:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">instance</span> <span>Arbitrary</span> <span>Foo</span> <span style="color:blue;font-weight:bold;">where</span>
  <span>arbitrary</span> <span style="color:red;">=</span> <span>oneof</span>
    <span style="color:red;">[</span> <span>Bar</span> <span>&lt;$&gt;</span> <span>arbitrary</span> <span>&lt;*&gt;</span> <span>arbitrary</span> <span>&lt;*&gt;</span> <span>arbitrary</span>
    <span style="color:red;">,</span> <span>Baz</span> <span>&lt;$&gt;</span> <span>arbitrary</span> <span>&lt;*&gt;</span> <span>arbitrary</span>
    <span style="color:red;">,</span> <span>Quux</span> <span>&lt;$&gt;</span> <span>arbitrary</span>
    <span style="color:red;">]</span>

<span style="color:blue;font-weight:bold;">instance</span> <span>Arbitrary</span> <span>Woz</span> <span style="color:blue;font-weight:bold;">where</span>
  <span>arbitrary</span> <span style="color:red;">=</span> <span>oneof</span>
    <span style="color:red;">[</span> <span>Wiz</span> <span>&lt;$&gt;</span> <span>arbitrary</span>
    <span style="color:red;">,</span> <span>Waz</span> <span>&lt;$&gt;</span> <span>arbitrary</span>
    <span style="color:red;">]</span></code></pre>
<p>This works reasonably well:</p>
<pre><code>λ&gt; sample (arbitrary :: Gen Foo)
Baz True True
Baz False True
Baz True True
Quux []
Baz False True
Bar '&lt;&#039; 3 &quot;zy\\\SOHpO_&quot;
Baz False True
Bar &#039;\SOH&#039; 0 &quot;\&quot;g\NAKm&quot;
Bar &#039;h&#039; (-9) &quot;(t&quot;
Quux [Wiz (-2),Waz False]
Baz False True</code></pre>
<p>The only problem is that writing those instances is quite tedious. There is no thought required at all. Isn’t this exactly the sort of thing that is supposed to be automated with <a href="https://wiki.haskell.org/GHC.Generics">generic programming</a>?</p>
<p>Why yes, yes it is. And the <code>generic-random</code> package can do exactly that. Notice that we have derived <code>Generic</code> for <code>Foo</code> and <code>Woz</code>. We can now use the <code>genericArbitrary</code> function from <code>Generic.Random.Generic</code> to derive completely standard <code>Arbitrary</code> instances, just like the ones we wrote above:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Arbitrary</span> <span>Foo</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>arbitrary</span> <span style="color:red;">=</span> <span>genericArbitrary</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Arbitrary</span> <span>Woz</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>arbitrary</span> <span style="color:red;">=</span> <span>genericArbitrary</span>
</code></pre>
<pre><code>λ&gt; sample (arbitrary :: Gen Foo)
Quux []
Bar '\159' (-2) ""
Baz True True
Baz False False
Baz True True
Baz True False
Quux [Wiz 9,Wiz 7,Waz True,Waz True,Waz False]
Quux [Wiz (-10),Waz False,Waz False,Waz True,Waz True,Wiz (-14),Wiz 13,Waz True,Wiz (-8),Wiz 12,Wiz (-13)]
Bar '\130' 10 "FN\222j?\b=\237(\NULW\231+ts\245"
Bar 'n' 14 ""
Bar '\205' 4 "\SYN"</code></pre>
<p>Seems about the same, except we wrote way less code! Huzzah!</p>
<p>If we want certain constructors to occur more frequently, we can also control that using <code>genericArbitraryFrequency</code>, which takes a list of <code>Int</code>s (each <code>Int</code> specifies the weight for one constructor).</p>
<p>A few notes:</p>
<ul>
<li><p>Using the <code>Generic.Random.Generic</code> module is the quickest and simplest way to generate random instances of your data type, if it works for your use case.</p></li>
<li><p>It has some limitations, namely:</p>
<ul>
<li><p>It <em>only</em> generates <code>Arbitrary</code> instances for QuickCheck. It can’t create more general random generators.</p></li>
<li><p>It probably won’t work very well for recursive data types.</p></li>
</ul></li>
</ul>
<p>However, these limitations are addressed by other parts of the library. Intrigued? Read on!</p>
<h2 id="recursive-types-the-simple-way">Recursive types, the simple way</h2>
<p>Let’s now consider a simple recursive type:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Tree</span> <span>a</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>Leaf</span>   <span style="color:red;">::</span> <span>a</span>                <span style="color:red;">-&gt;</span> <span>Tree</span> <span>a</span>
<span>&gt;</span>   <span>Branch</span> <span style="color:red;">::</span> <span>Tree</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Tree</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Tree</span> <span>a</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Generic</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>treeSize</span> <span style="color:red;">::</span> <span>Tree</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Int</span>
<span>&gt;</span> <span>treeSize</span> <span style="color:red;">(</span><span>Leaf</span> <span style="color:blue;font-weight:bold;">_</span><span style="color:red;">)</span>     <span style="color:red;">=</span> <span class="hs-num">1</span>
<span>&gt;</span> <span>treeSize</span> <span style="color:red;">(</span><span>Branch</span> <span>l</span> <span>r</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span class="hs-num">1</span> <span>+</span> <span>treeSize</span> <span>l</span> <span>+</span> <span>treeSize</span> <span>r</span>
</code></pre>
<p>We can try using <code>genericArbitrary</code>:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">instance</span> <span>Arbitrary</span> <span>a</span> <span style="color:red;">=&gt;</span> <span>Arbitrary</span> <span style="color:red;">(</span><span>Tree</span> <span>a</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">where</span>
  <span>arbitrary</span> <span style="color:red;">=</span> <span>genericArbitrary</span></code></pre>
<p>The problem is that this tends to generate some tiny trees and some enormous trees, with not much in between:</p>
<pre><code>λ&gt; map treeSize  replicateM 50 (generate (arbitrary :: Gen (Tree Int)))
[1,1,1,269,1,1,1,1,1,11,7,3,5,1,1,1,7,1,1,1,3,3,83,5,1,1,3,111,265,47,1,3,19,1,11,1,5,3,15,15,1,91,1,13,4097,119,1,15,5,3]</code></pre>
<p>And this is not a problem specific to trees; this kind of thing is likely to happen for any recursive type.</p>
<p>Before we get to more interesting/complicated tools, it’s worth noting that <code>random-generics</code> provides a simple mechanism to limit the size of the generated structures: the <code>genericArbitrary'</code> function works like <code>genericArbitrary</code> but uses QuickCheck’s <code>sized</code> mechanism to cut off the recursion when it gets too big. The available size is <em>partitioned</em> among recursive calls, so it does not suffer from the exponential growth you might see if only the <em>depth</em> was limited. When the size counter reaches zero, the generator tries to terminate the recursion by picking some finite, non-recursive value(s). The parameter to <code>genericArbitrary'</code> is a natural number specifying how deep the finite, recursion-terminating values can be. <code>Z</code> (<em>i.e</em> zero) means the generator will only be willing to terminate the recursion with nullary constructors. In our case, <code>Tree</code> does not have any nullary constructors, so we should not use <code>Z</code>: if we do, the generator will be unable to terminate the recursion when the size reaches zero and we will get the same behavior as <code>genericArbitrary</code>. Instead, we should use <code>S Z</code>, which means it will be able to pick the depth-1 term <code>Leaf x</code> (for some arbitrary <code>x</code>) to terminate the recursion.</p>
<p>Let’s try it:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span style="color:red;">(</span><span>Arbitrary</span> <span>a</span><span style="color:red;">,</span> <span>Generic</span> <span>a</span><span style="color:red;">,</span> <span>BaseCases</span> <span>Z</span> <span style="color:red;">(</span><span>Rep</span> <span>a</span><span style="color:red;">)</span><span style="color:red;">)</span> <span style="color:red;">=&gt;</span> <span>Arbitrary</span> <span style="color:red;">(</span><span>Tree</span> <span>a</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>arbitrary</span> <span style="color:red;">=</span> <span>genericArbitrary'</span> <span style="color:red;">(</span><span>S</span> <span>Z</span><span style="color:red;">)</span>
</code></pre>
<pre><code>λ&gt; sample (arbitrary :: Gen (Tree Int))
Leaf 0
Branch (Leaf 0) (Branch (Leaf 0) (Branch (Leaf 0) (Leaf 0)))
Branch (Leaf (-1)) (Leaf 1)
Leaf (-3)
Leaf 7
Branch (Leaf (-4)) (Branch (Branch (Leaf 1) (Leaf (-1))) (Leaf (-1)))
Branch (Leaf (-2)) (Branch (Leaf 1) (Branch (Leaf 0) (Branch (Leaf 0) (Leaf 0))))
Leaf 14
Branch (Branch (Leaf 2) (Leaf 2)) (Branch (Branch (Branch (Leaf 1) (Branch (Branch (Leaf 0) (Branch (Leaf 0) (Leaf 0))) (Branch (Leaf 0) (Leaf 0)))) (Branch (Branch (Branch (Leaf 0) (Leaf 0)) (Leaf 0)) (Leaf 0))) (Leaf (-3)))
Leaf 4
Leaf 9</code></pre>
<p>Ah, that’s much better.</p>
<p>Finally, <code>genericArbitraryFrequency'</code> is the same as <code>genericArbitraryFrequency</code> but limits the recursion depth as <code>genericArbitrary'</code> does.</p>
<p>If you have a recursive data type you want to use with QuickCheck, it’s worth trying this, since it is quick and simple. The main problem with this approach is that it does not generate a uniform distribution of values. (Also, it is limited in that it is specifically tied to QuickCheck.) In this example, although you can’t necessarily tell just by looking at the sample random trees, I guarantee you that some kinds of trees are much more likely to be generated than others. (Though I couldn’t necessarily tell you which kinds.) This can be bad if the specific trees that will trigger a bug are in fact unlikely to be generated.</p>
<p>Next time, we’ll look at how we can actually have efficient, size-limited, uniform random generators using <em>Boltzmann samplers</em>.</p>

