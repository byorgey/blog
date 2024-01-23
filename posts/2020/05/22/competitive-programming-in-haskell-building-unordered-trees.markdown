---
title: Competitive programming in Haskell: building unordered trees
published: 2020-05-22T22:17:00Z
categories: competitive programming,haskell
tags: equality,Kattis,multiset,parsec,parsing,set,shape,subway,tree
---

<p>In my previous post I challenged you to solve <a href="https://open.kattis.com/problems/subway">Subway Tree System</a>, which encodes trees by recording sequences of steps taken away from and towards the root while exploring the whole tree, and asks whether two such recordings denote the same tree. There are two main difficulties here: the first is how to do the parsing; second, how to compare two trees when we don’t care about the order of children at each node. Thanks to all of you who posted your solutions—I learned a lot. I often feel like my solution is obviously the “only” solution, but then when I see how others solve a problem I realize that the solution space is much larger than I thought!</p>
<h2 id="my-solution">My solution</h2>
<p>Here’s my solution, with some commentary interspersed. First, some pragmas and imports and such:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">{-# LANGUAGE OverloadedStrings #-}</span>
<span style="color:green;">{-# LANGUAGE TupleSections     #-}</span>

<span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Bool</span>
<span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.ByteString.Lazy.Char8</span>  <span style="color:blue;font-weight:bold;">as</span> <span>C</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Function</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.List</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.List.Split</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Map</span>                    <span style="color:red;">(</span><span>Map</span><span style="color:red;">)</span>
<span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.Map</span>                    <span style="color:blue;font-weight:bold;">as</span> <span>M</span>

<span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec.ByteString.Lazy</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec.Char</span></code></pre>
<p>My <code>main</code> then looks like this:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>main</span> <span style="color:red;">=</span> <span>C.interact</span> <span>$</span>
  <span>C.lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span>&gt;&gt;&gt;</span> <span>chunksOf</span> <span class="hs-num">2</span> <span>&gt;&gt;&gt;</span>
  <span>map</span> <span style="color:red;">(</span><span>solve</span> <span>&gt;&gt;&gt;</span> <span>bool</span> <span style="color:teal;">"different"</span> <span style="color:teal;">"same"</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>C.unlines</span></code></pre>
<p>The use of <code>ByteString</code> instead of <code>String</code> isn’t really necessary for this problem, just habit. I split the input into lines, group them in twos using <a href="http://hackage.haskell.org/package/split-0.2.3.4/docs/Data-List-Split.html#v:chunksOf"><code>Data.List.Split.chunksOf</code></a>, solve each test case, and turn the output into <code>different</code> or <code>same</code> appropriately. (<a href="https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Bool.html#v:bool"><code>Data.Bool.bool</code></a> is the fold/case analysis for the <code>Bool</code> type; I never use it in any other Haskell code but am unreasonably fond of it for this particular use case.) It would also be possible to use the <a href="https://byorgey.wordpress.com/2019/05/22/competitive-programming-in-haskell-scanner/"><code>Scanner</code> abstraction</a> instead of <code>lines</code>, <code>drop</code>, and <code>chunksOf</code>, as <a href="https://gist.github.com/blaisepascal/d1a5043e178f982f4484d3c188b4a634">commenter blaisepascal2014 did</a>. In some ways that would actually be nicer, but I often default to using these more basic tools in simple cases.</p>
<h2 id="parsing">Parsing</h2>
<p>Now for parsing the trees. The parsing is not <em>too</em> bad, and several commenters essentially did it manually with a recursive function manipulating a stack and so on; the most creative <a href="https://byorgey.wordpress.com/2020/05/19/competitive-programming-in-haskell-sorting-tree-shapes/#comment-38536">used a tree zipper</a> to literally walk around the tree being constructed, just like you are supposedly walking around a subway in the problem. However, the <code>parsec</code> package is available in the Kattis environment, so the easiest thing is to actually whip up a proper little parser. (I know of several other Kattis problems which can be nicely solved using parser combinators but would be annoying otherwise, for example, <a href="https://open.kattis.com/problems/calculator">Calculator</a> and <a href="https://open.kattis.com/problems/otpor">Otpor</a>. A rather fiendish but fun parsing puzzle is <a href="https://open.kattis.com/problems/learningtocode">Learning to Code</a>.)</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>readTree</span> <span style="color:red;">::</span> <span>C.ByteString</span> <span style="color:red;">-&gt;</span> <span>Tree</span>
<span>readTree</span> <span style="color:red;">=</span> <span>parse</span> <span>parseTree</span> <span style="color:teal;">""</span> <span>&gt;&gt;&gt;</span> <span>either</span> <span>undefined</span> <span>id</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>parseTree</span>    <span style="color:red;">=</span> <span>Node</span>     <span>&lt;$&gt;</span> <span>parseForest</span>
    <span>parseForest</span>  <span style="color:red;">=</span> <span>fromList</span> <span>&lt;$&gt;</span> <span>many</span> <span>parseSubtree</span>
    <span>parseSubtree</span> <span style="color:red;">=</span> <span>char</span> <span style="color:teal;">'0'</span> <span>*&gt;</span> <span>parseTree</span> <span>&lt;*</span> <span>char</span> <span style="color:teal;">'1'</span></code></pre>
<p>Of course I haven’t actually shown the definition of <code>Tree</code>, <code>Node</code>, or <code>fromList</code> yet, but hopefully you get the idea. <code>either undefined id</code> is <a href="https://byorgey.wordpress.com/2019/04/30/code-style-and-moral-absolutes/">justified here</a> since the input is guaranteed to be well-formed, so the parser will never actually fail with a <code>Left</code>.</p>
<h2 id="unordered-trees">Unordered trees</h2>
<p>The other difficulty is how to compare trees up to reordering children. Trying all permutations of the children at each node and seeing whether any match is obviously going to be much too slow! The key insight, and what this problem had in common with the one from <a href="https://byorgey.wordpress.com/2020/05/19/competitive-programming-in-haskell-sorting-tree-shapes/">my previous post</a>, is that we can use an (automatically-derived) <code>Ord</code> instance to <em>sort</em> the children at each node into a canonical order. We don’t really need to know or care <em>what</em> order they end up in, which depends on the precise details of how the derived <code>Ord</code> instance works. The point is that sorting into some consistent order allows us to efficiently test whether two lists are permutations of each other.</p>
<p>I think everyone who posted a solution created some kind of function to “canonicalize” a tree, by first canonicalizing all subtrees and then sorting them. When I first solved this problem, however, I approached it along slightly different lines, <a href="https://byorgey.wordpress.com/2020/05/19/competitive-programming-in-haskell-sorting-tree-shapes/#comment-38544">hinted at by commenter Globules</a>: can we define the <code>Tree</code> type in such a way that there is only a single representation for each tree-up-to-reordering?</p>
<p>My first idea was to use a <code>Data.Set</code> of children at each node, but this is subtly wrong, since it gets rid of duplicates! We don’t actually want a <em>set</em> of children at each node, but rather a <em>bag</em> (aka <em>multiset</em>). So I made a little <code>Bag</code> abstraction out of a <code>Map</code>. The magical thing is that GHC can still derive an <code>Ord</code> instance for my recursive tree type containing a newtype containing a <code>Map</code> containing trees! (OK, OK, it’s not really magic, but it still <em>feels</em> magic…)</p>
<p>Now, actually, I no longer think this is the <em>best</em> solution, but it’s interesting, so I’ll leave it. Later on I will show what I think is an even better solution.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">newtype</span> <span>Tree</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">(</span><span>Bag</span> <span>Tree</span><span style="color:red;">)</span>
  <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">)</span>

<span style="color:blue;font-weight:bold;">newtype</span> <span>Bag</span> <span>a</span> <span style="color:red;">=</span> <span>Bag</span> <span style="color:red;">(</span><span>Map</span> <span>a</span> <span>Int</span><span style="color:red;">)</span>
  <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">)</span>

<span>fromList</span> <span style="color:red;">::</span> <span>Ord</span> <span>a</span> <span style="color:red;">=&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Bag</span> <span>a</span>
<span>fromList</span> <span style="color:red;">=</span> <span>map</span> <span style="color:red;">(</span><span style="color:red;">,</span><span class="hs-num">1</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>M.fromListWith</span> <span style="color:red;">(</span><span>+</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>Bag</span></code></pre>
<p>The final piece is the <code>solve</code> function, which simply calls <code>readTree</code> on the two strings and compares the resulting (canonical!) <code>Tree</code> values for equality.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>C.ByteString</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Bool</span>
<span>solve</span> <span style="color:red;">[</span><span>t1</span><span style="color:red;">,</span><span>t2</span><span style="color:red;">]</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>==</span><span style="color:red;">)</span> <span>`on`</span> <span>readTree</span><span style="color:red;">)</span> <span>t1</span> <span>t2</span></code></pre>
<h2 id="a-better-way">A better way</h2>
<p>I still think it’s a nice idea to have canonical-by-construction trees, rather than building ordered trees and then calling a separate function to canonicalize them afterwards. But inspired by several commenters’ solutions, I realized that rather than my complex <code>Bag</code> type, it’s much nicer to simply use a <em>sorted list</em> as the canonical representation of a <code>Node</code>’s bag of subtrees, and to use a smart constructor to build them:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">newtype</span> <span>Tree</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">[</span><span>Tree</span><span style="color:red;">]</span>
  <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">)</span>

<span>mkNode</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Tree</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Tree</span>
<span>mkNode</span> <span style="color:red;">=</span> <span>Node</span> <span>.</span> <span>sort</span></code></pre>
<p>Then we just use <code>mkNode</code> instead of <code>Node</code> in the parser, and voilà! The canonicalization happens on the fly while parsing the tree. By contrast, if we write a separate canonicalization function, like</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>canonical</span> <span style="color:red;">::</span> <span>Tree</span> <span style="color:red;">-&gt;</span> <span>Tree</span>
<span>canonical</span> <span style="color:red;">(</span><span>Node</span> <span>ts</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">(</span><span>map</span> <span>canonical</span> <span style="color:red;">(</span><span>sort</span> <span>ts</span><span style="color:red;">)</span><span style="color:red;">)</span></code></pre>
<p>it is actually possible to get it wrong. In fact, I deliberately introduced a bug into the above function: can you see what it is?</p>
<p>All told, then, here is the (in my opinion) nicest solution that I know of:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">{-# LANGUAGE OverloadedStrings #-}</span>

<span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Bool</span>
<span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.ByteString.Lazy.Char8</span> <span style="color:blue;font-weight:bold;">as</span> <span>C</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Function</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.List</span>

<span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>ScannerBS</span>                  <span>hiding</span> <span style="color:red;">(</span><span>many</span><span style="color:red;">)</span>

<span>main</span> <span style="color:red;">=</span> <span>C.interact</span> <span>$</span>
  <span>runScanner</span> <span style="color:red;">(</span><span>numberOf</span> <span style="color:red;">(</span><span>two</span> <span>str</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span>
  <span>map</span> <span style="color:red;">(</span><span>solve</span> <span>&gt;&gt;&gt;</span> <span>bool</span> <span style="color:teal;">"different"</span> <span style="color:teal;">"same"</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>C.unlines</span>

<span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>C.ByteString</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Bool</span>
<span>solve</span> <span style="color:red;">[</span><span>t1</span><span style="color:red;">,</span><span>t2</span><span style="color:red;">]</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>==</span><span style="color:red;">)</span> <span>`on`</span> <span>readTree</span><span style="color:red;">)</span> <span>t1</span> <span>t2</span>

<span style="color:blue;font-weight:bold;">newtype</span> <span>Tree</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">[</span><span>Tree</span><span style="color:red;">]</span> <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">)</span>

<span>readTree</span> <span style="color:red;">::</span> <span>C.ByteString</span> <span style="color:red;">-&gt;</span> <span>Tree</span>
<span>readTree</span> <span style="color:red;">=</span> <span>parse</span> <span>parseTree</span> <span style="color:teal;">""</span> <span>&gt;&gt;&gt;</span> <span>either</span> <span>undefined</span> <span>id</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>parseTree</span>    <span style="color:red;">=</span> <span style="color:red;">(</span><span>Node</span> <span>.</span> <span>sort</span><span style="color:red;">)</span> <span>&lt;$&gt;</span> <span>many</span> <span>parseSubtree</span>
    <span>parseSubtree</span> <span style="color:red;">=</span> <span>char</span> <span style="color:teal;">'0'</span> <span>*&gt;</span> <span>parseTree</span> <span>&lt;*</span> <span>char</span> <span style="color:teal;">'1'</span></code></pre>
<h2 id="next-problem">Next problem</h2>
<p>For Tuesday, I invite you to solve <a href="https://open.kattis.com/problems/substitution">The Power of Substitution</a>. Don’t let the high difficulty rating scare you; in my estimation it should be quite accessible if you know a bit of math and have been following along with some of my previous posts (YMMV). However, it’s not quite as obvious what the nicest way to write it in Haskell is.</p>

