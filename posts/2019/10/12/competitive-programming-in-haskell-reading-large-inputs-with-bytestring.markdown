---
katex: true
title: 'Competitive Programming in Haskell: reading large inputs with ByteString'
published: 2019-10-13T02:46:38Z
categories: haskell
tags: ByteString,competitive,parsing,programming,Scanner
---

<p>In my <a href="https://byorgey.wordpress.com/2019/05/22/competitive-programming-in-haskell-scanner/">last post</a> in this series, we looked at building a small <code>Scanner</code> combinator library for lightweight input parsing. It uses <code>String</code> everywhere, and usually this is fine, but occasionally it’s not.</p>
<p>A good example is the Kattis problem <a href="https://open.kattis.com/problems/armystrengthhard">Army Strength (Hard)</a>. There are a number of separate test cases; each test case consists of two lines of positive integers which record the strengths of monsters in two different armies. Supposedly the armies will have a sequence of battles, where the weakest monster dies each time, with some complex-sounding rules about how to break ties. It sounds way more complicated than it really is, though: a bit of thought reveals that to find out who wins we really just need to see which army’s maximum-strength monster is strongest.</p>
<p>So our strategy for each test case is to read in the two lists of integers, find the maximum of each list, and compare. Seems pretty straightforward, right? Something like this:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.List.Split</span>

<span>main</span> <span style="color:red;">=</span> <span>interact</span> <span>$</span>
  <span>lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span>&gt;&gt;&gt;</span> <span>chunksOf</span> <span class="hs-num">4</span> <span>&gt;&gt;&gt;</span>
  <span>map</span> <span style="color:red;">(</span><span>drop</span> <span class="hs-num">2</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>words</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span>read</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>solve</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span>
  <span>unlines</span>

<span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>String</span>
<span>solve</span> <span style="color:red;">[</span><span>gz</span><span style="color:red;">,</span> <span>mgz</span><span style="color:red;">]</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">case</span> <span>compare</span> <span style="color:red;">(</span><span>maximum</span> <span>gz</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>maximum</span> <span>mgz</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">of</span>
  <span>LT</span> <span style="color:red;">-&gt;</span> <span style="color:teal;">"MechaGodzilla"</span>
  <span style="color:blue;font-weight:bold;">_</span>  <span style="color:red;">-&gt;</span> <span style="color:teal;">"Godzilla"</span></code></pre>
<p>Note I didn’t actually use the <code>Scanner</code> abstraction here, though I could have; it’s actually easier to just ignore the numbers telling us how many test cases there are and the length of each line, and just split up the input by lines and go from there.</p>
<p>This seems straightforward enough, but sadly, it results in a Time Limit Exceeded (TLE) error on the third of three test cases. Apparently this program takes longer than the allowed 1 second. What’s going on?</p>
<p>If we look carefully at the limits for the problem, we see that there could be up to 50 test cases, each test case could have two lists of length $10^5$, and the numbers in the lists can be up to $10^9$. If all those are maxed out (as they probably are in the third, secret test case), we are looking at an input file many megabytes in size. At this point the time to simply read the input is a big factor. Reading the input as a <code>String</code> has a lot of overhead: each character gets its own cons cell; breaking the input into lines and words requires traversing over these cons cells one by one. We need a representation with less overhead.</p>
<p>Now, if this were a <em>real</em> application, we would reach for <code>Text</code>, which is made for representing textual information and can correctly handle unicode encodings and all that good stuff. However, this isn’t a real application: competitive programming problems <em>always</em> limit the input and output strictly to ASCII, so characters are synonymous with bytes. Therefore we will commit a “double no-no”: not only are we going to use <code>ByteString</code> to represent text, we’re going to use <code>Data.ByteString.Lazy.Char8</code> which simply assumes that each 8 bits is one character. As <a href="https://byorgey.wordpress.com/2019/04/30/code-style-and-moral-absolutes/">explained in a previous post</a>, however, I think this is one of those things that is usually a no-no but is completely justified in this context.</p>
<p>Let’s start by just replacing some of our string manipulation with corresponding <code>ByteString</code> versions:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.ByteString.Lazy.Char8</span> <span style="color:blue;font-weight:bold;">as</span> <span>C</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.List.Split</span>

<span>main</span> <span style="color:red;">=</span> <span>C.interact</span> <span>$</span>
  <span>C.lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span>&gt;&gt;&gt;</span> <span>chunksOf</span> <span class="hs-num">4</span> <span>&gt;&gt;&gt;</span>
  <span>map</span> <span style="color:red;">(</span><span>drop</span> <span class="hs-num">2</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>C.words</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>C.unpack</span> <span>&gt;&gt;&gt;</span> <span>read</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>solve</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span>
  <span>C.unlines</span>

<span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>C.ByteString</span>
<span>solve</span> <span style="color:red;">[</span><span>gz</span><span style="color:red;">,</span> <span>mgz</span><span style="color:red;">]</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">case</span> <span>compare</span> <span style="color:red;">(</span><span>maximum</span> <span>gz</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>maximum</span> <span>mgz</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">of</span>
  <span>LT</span> <span style="color:red;">-&gt;</span> <span>C.pack</span> <span style="color:teal;">"MechaGodzilla"</span>
  <span style="color:blue;font-weight:bold;">_</span>  <span style="color:red;">-&gt;</span> <span>C.pack</span> <span style="color:teal;">"Godzilla"</span></code></pre>
<p>This already helps a lot: this version is actually accepted, taking 0.66 seconds. (Note there’s no way to find out how long our first solution <em>would</em> take if allowed to run to completion: once it goes over the time limit Kattis just kills the process. So we really don’t know how much of an improvement this is, but hey, it’s accepted!)</p>
<p>But we can do even better: it turns out that <code>read</code> also has a lot of overhead, and if we are specifically reading <code>Int</code> values we can do something much better. The <code>ByteString</code> module comes with a function</p>
<p><code>readInt :: C.ByteString -&gt; Maybe (Int, C.ByteString)</code></p>
<p>Since, in this context, we know we will always get an integer with nothing left over, we can replace <code>C.unpack &gt;&gt;&gt; read</code> with <code>C.readInt &gt;&gt;&gt; fromJust &gt;&gt;&gt; fst</code>. Let’s try it:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.ByteString.Lazy.Char8</span> <span style="color:blue;font-weight:bold;">as</span> <span>C</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.List.Split</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Maybe</span> <span style="color:red;">(</span><span>fromJust</span><span style="color:red;">)</span>

<span>main</span> <span style="color:red;">=</span> <span>C.interact</span> <span>$</span>
  <span>C.lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span>&gt;&gt;&gt;</span> <span>chunksOf</span> <span class="hs-num">4</span> <span>&gt;&gt;&gt;</span>
  <span>map</span> <span style="color:red;">(</span><span>drop</span> <span class="hs-num">2</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>C.words</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span>readInt</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>solve</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span>
  <span>C.unlines</span>

  <span style="color:blue;font-weight:bold;">where</span>
    <span>readInt</span> <span style="color:red;">=</span> <span>C.readInt</span> <span>&gt;&gt;&gt;</span> <span>fromJust</span> <span>&gt;&gt;&gt;</span> <span>fst</span>

<span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>C.ByteString</span>
<span>solve</span> <span style="color:red;">[</span><span>gz</span><span style="color:red;">,</span> <span>mgz</span><span style="color:red;">]</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">case</span> <span>compare</span> <span style="color:red;">(</span><span>maximum</span> <span>gz</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>maximum</span> <span>mgz</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">of</span>
  <span>LT</span> <span style="color:red;">-&gt;</span> <span>C.pack</span> <span style="color:teal;">"MechaGodzilla"</span>
  <span style="color:blue;font-weight:bold;">_</span>  <span style="color:red;">-&gt;</span> <span>C.pack</span> <span style="color:teal;">"Godzilla"</span></code></pre>
<p>Now we’re talking — this version completes in a blazing 0.04 seconds!</p>
<p>We can take these principles and use them to make a variant of the <code>Scanner</code> module from last time which uses (lazy, ASCII) <code>ByteString</code> instead of <code>String</code>, including the use of the <code>readInt</code> functions to read <code>Int</code> values quickly. You can <a href="https://github.com/byorgey/comprog-hs/blob/master/ScannerBS.hs">find it here</a>.</p>

