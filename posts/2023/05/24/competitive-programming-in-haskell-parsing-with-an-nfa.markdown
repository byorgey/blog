---
katex: true
title: 'Competitive programming in Haskell: parsing with an NFA'
published: 2023-05-24T11:03:02Z
categories: competitive programming,haskell
tags: challenge,Kattis,NFA,parsing
---

<p>In <a href="https://byorgey.github.io/blog/posts/2023/05/03/competitive-programming-in-haskell-tries.html">my previous post</a>, I challenged you to solve <a href="https://open.kattis.com/problems/chemistsvows">Chemist’s Vows</a>. In this problem, we have to decide which words can be made by concatenating atomic element symbols. So this is another parsing problem; but unlike the <a href="https://byorgey.github.io/blog/posts/2023/05/03/competitive-programming-in-haskell-tries.html">previous problem</a>, element symbols are not prefix-free. For example, <code>B</code> and <code>Be</code> are both element symbols. So, if we see <code>BE...</code>, we don’t immediately know whether we should parse it as <code>Be</code>, or as <code>B</code> followed by an element that starts with <code>E</code> (such as <code>Er</code>).</p>
<h2 id="a-first-try">A first try</h2>
<p>A parsing problem, eh? Haskell actually shines in this area because of its nice parser combinator libraries. The Kattis environment does in fact have the <code>parsec</code> package available; and even on platforms that don’t have <code>parsec</code>, we can always use the <code>Text.ParserCombinators.ReadP</code> module that comes in <code>base</code>. So let’s try throwing one of those packages at the problem and see what happens!</p>
<p>If we try using <code>parsec</code>, we immediately run into problems; honestly, I don’t even know how to solve the problem using <code>parsec</code>. The problem is that <code>&lt;|&gt;</code> represents <em>left-biased</em> choice. If we parse <code>p1 &lt;|&gt; p2</code> and parser <code>p1</code> succeeds, then we will <em>never consider</em> <code>p2</code>. But for this parsing problem, because the symbols are not prefix-free, sometimes we can’t know which of two options we should have picked until later.</p>
<p><code>ReadP</code>, on the other hand, explicitly has both biased and unbiased choice operators, and can return a <em>list of possible parses</em> instead of just a single parse. That sounds promising! Here’s a simple attempt using <code>ReadP</code>: to parse a single element, we use an unbiased <code>choice</code> over all the element names; then we use <code>many parseElement &lt;* eof</code> to parse each word, and check whether there are any successful parses at all.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color: green">{-# LANGUAGE OverloadedStrings #-}</span>

<span style="color: blue;font-weight: bold">import</span>           <span>Control.Arrow</span>
<span style="color: blue;font-weight: bold">import</span>           <span>Data.Bool</span>
<span style="color: blue;font-weight: bold">import</span> <span style="color: blue;font-weight: bold">qualified</span> <span>Data.ByteString.Lazy.Char8</span>   <span style="color: blue;font-weight: bold">as</span> <span>C</span>
<span style="color: blue;font-weight: bold">import</span>           <span>Text.ParserCombinators.ReadP</span> <span style="color: red">(</span><span>ReadP</span><span style="color: red">,</span> <span>choice</span><span style="color: red">,</span> <span>eof</span><span style="color: red">,</span> <span>many</span><span style="color: red">,</span>
                                               <span>readP_to_S</span><span style="color: red">,</span> <span>string</span><span style="color: red">)</span>

<span>main</span> <span style="color: red">=</span> <span>C.interact</span> <span>$</span>
  <span>C.lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color: red">(</span><span>solve</span> <span>&gt;&gt;&gt;</span> <span>bool</span> <span style="color: teal">"NO"</span> <span style="color: teal">"YES"</span><span style="color: red">)</span> <span>&gt;&gt;&gt;</span> <span>C.unlines</span>

<span>solve</span> <span style="color: red">::</span> <span>C.ByteString</span> <span style="color: red">-&gt;</span> <span>Bool</span>
<span>solve</span> <span>s</span> <span style="color: red">=</span> <span style="color: blue;font-weight: bold">case</span> <span>readP_to_S</span> <span style="color: red">(</span><span>many</span> <span>parseElement</span> <span>&lt;*</span> <span>eof</span><span style="color: red">)</span> <span style="color: red">(</span><span>C.unpack</span> <span>s</span><span style="color: red">)</span> <span style="color: blue;font-weight: bold">of</span>
  <span>[]</span> <span style="color: red">-&gt;</span> <span>False</span>
  <span style="color: blue;font-weight: bold">_</span>  <span style="color: red">-&gt;</span> <span>True</span>

<span>elements</span> <span style="color: red">::</span> <span style="color: red">[</span><span>String</span><span style="color: red">]</span>
<span>elements</span> <span style="color: red">=</span> <span>words</span> <span>$</span>
  <span style="color: teal">"h he li be b c n o f ne na mg al si p s cl ar k ca sc ti v cr mn fe co ni cu zn ga ge as se br kr rb sr y zr nb mo tc ru rh pd ag cd in sn sb te i xe cs ba hf ta w re os ir pt au hg tl pb bi po at rn fr ra rf db sg bh hs mt ds rg cn fl lv la ce pr nd pm sm eu gd tb dy ho er tm yb lu ac th pa u np pu am cm bk cf es fm md no lr"</span>

<span>parseElement</span> <span style="color: red">::</span> <span>ReadP</span> <span>String</span>
<span>parseElement</span> <span style="color: red">=</span> <span>choice</span> <span style="color: red">(</span><span>map</span> <span>string</span> <span>elements</span><span style="color: red">)</span></code></pre>
<p>Unfortunately, this fails with a <em>Time Limit Exceeded</em> error (it takes longer than the allotted 5 seconds). The problem is that backtracking and trying every possible parse like this is super inefficient. One of the secret test inputs is almost cerainly constructed so that there are an exponential number of ways to parse some <em>prefix</em> of the input, but no way to parse the entire thing. As a simple example, the string <code>crf</code> can be parsed as either <code>c rf</code> (carbon + rutherfordium) or <code>cr f</code> (chromium + fluorine), so by repeating <code>crf</code> $n$ times we can make a string of length $3n$ which has $2^n$ different parses. If we fed this string to the <code>ReadP</code> solution above, it would quickly succeed with more or less the first thing that it tried. However, if we stick a letter on the end that does not occur in any element symbol (such as <code>q</code>), the result will be an unparseable string, and the <code>ReadP</code> solution will spend a very long time backtracking through exponentially many parses that all ultimately fail.</p>
<h2 id="solution">Solution</h2>
<p>The key insight is that we don’t really care about all the different possible parses; we only care whether the given string is parseable at all. At any given point in the string, there are only two possible states we could be in: we could be finished reading one element symbol and about to start reading the next one, or we could be in the middle of reading a two-letter element symbol. We can just scan through the string and keep track of the set of (at most two) possible states; in other words, we will simulate an <a href="https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton">NFA</a> which accepts the language of strings composed of element symbols.</p>
<p>First, some setup as before.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color: green">{-# LANGUAGE OverloadedStrings #-}</span>

<span style="color: blue;font-weight: bold">import</span>           <span>Control.Arrow</span>              <span style="color: red">(</span><span style="color: red">(</span><span>&gt;&gt;&gt;</span><span style="color: red">)</span><span style="color: red">)</span>
<span style="color: blue;font-weight: bold">import</span>           <span>Data.Array</span>                 <span style="color: red">(</span><span>Array</span><span style="color: red">,</span> <span>accumArray</span><span style="color: red">,</span> <span style="color: red">(</span><span>!</span><span style="color: red">)</span><span style="color: red">)</span>
<span style="color: blue;font-weight: bold">import</span>           <span>Data.Bool</span>                  <span style="color: red">(</span><span>bool</span><span style="color: red">)</span>
<span style="color: blue;font-weight: bold">import</span> <span style="color: blue;font-weight: bold">qualified</span> <span>Data.ByteString.Lazy.Char8</span> <span style="color: blue;font-weight: bold">as</span> <span>C</span>
<span style="color: blue;font-weight: bold">import</span>           <span>Data.List</span>                  <span style="color: red">(</span><span>partition</span><span style="color: red">,</span> <span>nub</span><span style="color: red">)</span>
<span style="color: blue;font-weight: bold">import</span>           <span>Data.Set</span>                   <span style="color: red">(</span><span>Set</span><span style="color: red">)</span>
<span style="color: blue;font-weight: bold">import</span> <span style="color: blue;font-weight: bold">qualified</span> <span>Data.Set</span>                   <span style="color: blue;font-weight: bold">as</span> <span>S</span>

<span>main</span> <span style="color: red">=</span> <span>C.interact</span> <span>$</span>
  <span>C.lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color: red">(</span><span>solve</span> <span>&gt;&gt;&gt;</span> <span>bool</span> <span style="color: teal">"NO"</span> <span style="color: teal">"YES"</span><span style="color: red">)</span> <span>&gt;&gt;&gt;</span> <span>C.unlines</span>

<span>elements</span> <span style="color: red">::</span> <span style="color: red">[</span><span>String</span><span style="color: red">]</span>
<span>elements</span> <span style="color: red">=</span> <span>words</span> <span>$</span>
  <span style="color: teal">"h he li be b c n o f ne na mg al si p s cl ar k ca sc ti v cr mn
fe co ni cu zn ga ge as se br kr rb sr y zr nb mo tc ru rh pd ag cd
in sn sb te i xe cs ba hf ta w re os ir pt au hg tl pb bi po at rn
fr ra rf db sg bh hs mt ds rg cn fl lv la ce pr nd pm sm eu gd tb dy
ho er tm yb lu ac th pa u np pu am cm bk cf es fm md no lr"</span></code></pre>
<p>Now, let’s split the element symbols into one-letter and two-letter symbols:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>singles</span><span style="color: red">,</span> <span>doubles</span> <span style="color: red">::</span> <span style="color: red">[</span><span>String</span><span style="color: red">]</span>
<span style="color: red">(</span><span>singles</span><span style="color: red">,</span> <span>doubles</span><span style="color: red">)</span> <span style="color: red">=</span> <span>partition</span> <span style="color: red">(</span><span style="color: red">(</span><span>==</span><span class="hs-num">1</span><span style="color: red">)</span><span>.</span><span>length</span><span style="color: red">)</span> <span>elements</span></code></pre>
<p>We can now make boolean lookup arrays that tell us whether a given letter occurs as a single-letter element symbol (<code>single</code>) and whether a given letter occurs as the first letter of a two-letter symbol (<code>lead</code>). We also make a <code>Set</code> of all two-letter element symbols, for fast lookup.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>mkAlphaArray</span> <span style="color: red">::</span> <span style="color: red">[</span><span>Char</span><span style="color: red">]</span> <span style="color: red">-&gt;</span> <span>Array</span> <span>Char</span> <span>Bool</span>
<span>mkAlphaArray</span> <span>cs</span> <span style="color: red">=</span> <span>accumArray</span> <span style="color: red">(</span><span>||</span><span style="color: red">)</span> <span>False</span> <span style="color: red">(</span><span style="color: teal">'a'</span><span style="color: red">,</span> <span style="color: teal">'z'</span><span style="color: red">)</span> <span style="color: red">(</span><span>zip</span> <span>cs</span> <span style="color: red">(</span><span>repeat</span> <span>True</span><span style="color: red">)</span><span style="color: red">)</span>

<span>single</span><span style="color: red">,</span> <span>lead</span> <span style="color: red">::</span> <span>Array</span> <span>Char</span> <span>Bool</span>
<span style="color: red">[</span><span>single</span><span style="color: red">,</span> <span>lead</span><span style="color: red">]</span> <span style="color: red">=</span> <span>map</span> <span style="color: red">(</span><span>mkAlphaArray</span> <span>.</span> <span>map</span> <span>head</span><span style="color: red">)</span> <span style="color: red">[</span><span>singles</span><span style="color: red">,</span> <span>doubles</span><span style="color: red">]</span>

<span>doubleSet</span> <span style="color: red">::</span> <span>Set</span> <span>String</span>
<span>doubleSet</span> <span style="color: red">=</span> <span>S.fromList</span> <span>doubles</span></code></pre>
<p>Now for simulating the NFA itself. There are two states we can be in: <code>START</code> means we are about to start and/or have just finished reading an element symbol; <code>SEEN c</code> means we have seen the first character of some element (<code>c</code>) and are waiting to see another.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color: blue;font-weight: bold">data</span> <span>State</span> <span style="color: red">=</span> <span>START</span> <span style="color: red">|</span> <span>SEEN</span> <span>Char</span>
  <span style="color: blue;font-weight: bold">deriving</span> <span style="color: red">(</span><span>Eq</span><span style="color: red">,</span> <span>Ord</span><span style="color: red">,</span> <span>Show</span><span style="color: red">)</span></code></pre>
<p>Our transition function takes a character <code>c</code> and a state and returns a set of all possible next states (we just use a list since these sets will be very small). If we are in the <code>START</code> state, we could end up in the <code>START</code> state again if <code>c</code> is a single-letter element symbol; we could also end up in the <code>SEEN c</code> state if <code>c</code> is the first letter of any two-letter element symbol. On the other hand, if we are in the <code>SEEN x</code> state, then we have to check whether <code>xc</code> is a valid element symbol; if so, we return to <code>START</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>delta</span> <span style="color: red">::</span> <span>Char</span> <span style="color: red">-&gt;</span> <span>State</span> <span style="color: red">-&gt;</span> <span style="color: red">[</span><span>State</span><span style="color: red">]</span>
<span>delta</span> <span>c</span> <span>START</span>    <span style="color: red">=</span> <span style="color: red">[</span><span>START</span> <span style="color: red">|</span> <span>single</span><span>!</span><span>c</span><span style="color: red">]</span> <span>++</span> <span style="color: red">[</span><span>SEEN</span> <span>c</span> <span style="color: red">|</span> <span>lead</span><span>!</span><span>c</span><span style="color: red">]</span>
<span>delta</span> <span>c</span> <span style="color: red">(</span><span>SEEN</span> <span>x</span><span style="color: red">)</span> <span style="color: red">=</span> <span style="color: red">[</span><span>START</span> <span style="color: red">|</span> <span style="color: red">[</span><span>x</span><span style="color: red">,</span><span>c</span><span style="color: red">]</span> <span>`S.member`</span> <span>doubleSet</span><span style="color: red">]</span></code></pre>
<p>We can now extend <code>delta</code> to act on a set of states, giving us the set of all possible resulting states; the <code>drive</code> function then iterates this one-letter transition over an entire input string. Finally, to solve the problem, we start with the singleton set <code>[START]</code>, call <code>drive</code> using the input string, and check whether <code>START</code> (which is also the only accepting state) is an element of the resulting set of states.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>trans</span> <span style="color: red">::</span> <span>Char</span> <span style="color: red">-&gt;</span> <span style="color: red">[</span><span>State</span><span style="color: red">]</span> <span style="color: red">-&gt;</span> <span style="color: red">[</span><span>State</span><span style="color: red">]</span>
<span>trans</span> <span>c</span> <span>sts</span> <span style="color: red">=</span> <span>nub</span> <span style="color: red">(</span><span>sts</span> <span>&gt;&gt;=</span> <span>delta</span> <span>c</span><span style="color: red">)</span>

<span>drive</span> <span style="color: red">::</span> <span>C.ByteString</span> <span style="color: red">-&gt;</span> <span style="color: red">(</span><span style="color: red">[</span><span>State</span><span style="color: red">]</span> <span style="color: red">-&gt;</span> <span style="color: red">[</span><span>State</span><span style="color: red">]</span><span style="color: red">)</span>
<span>drive</span> <span style="color: red">=</span> <span>C.foldr</span> <span style="color: red">(</span><span style="color: red">\</span><span>c</span> <span style="color: red">-&gt;</span> <span style="color: red">(</span><span>trans</span> <span>c</span> <span>&gt;&gt;&gt;</span><span style="color: red">)</span><span style="color: red">)</span> <span>id</span>

<span>solve</span> <span style="color: red">::</span> <span>C.ByteString</span> <span style="color: red">-&gt;</span> <span>Bool</span>
<span>solve</span> <span>s</span> <span style="color: red">=</span> <span>START</span> <span>`elem`</span> <span>drive</span> <span>s</span> <span style="color: red">[</span><span>START</span><span style="color: red">]</span></code></pre>
<p>And that’s it! This solution is accepted in 0.27 seconds (out of a maximum allowed 5 seconds).</p>
<h2 id="for-next-time">For next time</h2>
<ul>
<li>If you want to practice the concepts from my past couple posts, give <a href="https://open.kattis.com/problems/haiku">Haiku</a> a try.</li>
<li>For my next post, I challenge you to solve <a href="https://open.kattis.com/problems/zapis">Zapis</a>!</li>
</ul>

