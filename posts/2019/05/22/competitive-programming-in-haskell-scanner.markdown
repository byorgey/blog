---
katex: true
title: 'Competitive Programming in Haskell: Scanner'
published: 2019-05-22T17:58:00Z
categories: haskell
tags: competitive,parsing,programming,Scanner
---

<p>In my <a href="https://byorgey.github.io/blog/posts/2019/04/24/competitive-programming-in-haskell-basic-setup.html">previous post</a> I explored solving a simple competitive programming problem in Haskell. The input of the problem just consisted of a bunch of lines containing specific data, so that we could parse it using <code>lines</code> and <code>words</code>. There is another common class of problems, however, which follow this pattern:</p>
<p><em>The first line of the input consists of an integer $T$. Each of the next $T$ lines consists of…</em></p>
<p>That is, the input contains integers which are not input data per se but just tell you how many things are to follow. This is really easy to process in an imperative language like Java or C++. For example, in Java we might write code like this:</p>
<div class="sourceCode"><pre class="sourceCode java"><code class="sourceCode java"><span class="bu">Scanner</span> in = <span class="kw">new</span> <span class="bu">Scanner</span>(<span class="bu">System</span>.<span class="fu">in</span>);
<span class="dt">int</span> T = in.<span class="fu">nextInt</span>();
<span class="kw">for</span> (<span class="dt">int</span> i = <span class="dv">0</span>; i &lt; T; i++) {
   <span class="co">// process each line</span>
}</code></pre></div>
<p>Occasionally, we can get away with completely ignoring the extra information in Haskell. For example, if the input consists of a number $T$ followed by $T$ lines, each of which contains a number $n$ followed by a list of $n$ numbers, we can just write</p>
<pre><code>main = interact $
  lines &gt;&gt;&gt; drop 1 &gt;&gt;&gt; map (words &gt;&gt;&gt; drop 1 &gt;&gt;&gt; map read) &gt;&gt;&gt; ...</code></pre>
<p>That is, we can ignore the first line containing $T$ since the end-of-file will tell us how many lines there are; and we can ignore the $n$ at the beginning of each line, since the newline character tells us when the list on that line is done.</p>
<p>Sometimes, however, this isn’t possible, especially when there are multiple test cases, or when a single test case has multiple parts, each of which can have a variable length. For example, consider <a href="https://open.kattis.com/problems/vote">Popular Vote</a>, which describes its input as follows:</p>
<blockquote>
The first line of input contains a single positive integer $T \leq 500$ indicating the number of test cases. The first line of each test case also contains a single positive integer $n$ indicating the number of candidates in the election. This is followed by $n$ lines, with the $i$th line containing a single nonnegative integer indicating the number of votes candidate $i$ received.
</blockquote>
<p>How would we parse this? We could still ignore $T$—just keep reading until the end of the file—but there’s no way we can ignore the $n$ values. Since the values for each test case are all on separate lines instead of on one line, there’s otherwise no way to know when one test case ends and the next begins.</p>
<p>Once upon a time, I would have done this using <code>splitAt</code> and explicit recursion, like so:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">type</span> <span>Election</span> <span style="color:red;">=</span> <span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span>

<span>readInput</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Election</span><span style="color:red;">]</span>
<span>readInput</span> <span style="color:red;">=</span> <span>lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span style="color:green;">{- ignore T -}</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span>read</span> <span>&gt;&gt;&gt;</span> <span>go</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>go</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Election</span><span style="color:red;">]</span>
    <span>go</span> <span>[]</span>     <span style="color:red;">=</span> <span>[]</span>
    <span>go</span> <span style="color:red;">(</span><span>n</span><span>:</span><span>xs</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>votes</span> <span>:</span> <span>go</span> <span>rest</span>
      <span style="color:blue;font-weight:bold;">where</span> <span style="color:red;">(</span><span>votes</span><span style="color:red;">,</span><span>rest</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>splitAt</span> <span>n</span> <span>xs</span></code></pre>
<p>However, this is really annoying to write and easy to get wrong. There are way too many variable names to keep track of (<code>n</code>, <code>xs</code>, <code>votes</code>, <code>rest</code>, <code>go</code>) and for more complex inputs it becomes simply unmanageable. You might think we should switch to using a real parser combinator library—<code>parsec</code> is indeed installed in the environment Kattis uses to run Haskell solutions—and although sometimes a full-blown parser combinator library is needed, in this case it’s quite a bit more heavyweight than we would like. I can never remember which modules I have to import to get <code>parsec</code> set up; there’s a bunch of boilerplate needed to set up a lexer; and so on. Using <code>parsec</code> is only worth it if we’re parsing something really complex.</p>
<h2 id="scanner">Scanner</h2>
<p>The heart of the issue is that we want to be able to specify a high-level description of the sequence of things we expect to see in the input, without worrying about managing the stream of tokens explicitly. Another key insight is that 99% of the time, we <em>don’t</em> need the ability to deal with parse failure or the ability to parse multiple alternatives. With these insights in mind, we can create a very simple <code>Scanner</code> abstraction, which is just a <code>State</code>ful computation over a list of tokens:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">type</span> <span>Scanner</span> <span style="color:red;">=</span> <span>State</span> <span style="color:red;">[</span><span>String</span><span style="color:red;">]</span>

<span>runScanner</span> <span style="color:red;">::</span> <span>Scanner</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>a</span>
<span>runScanner</span> <span>s</span> <span style="color:red;">=</span> <span>evalState</span> <span>s</span> <span>.</span> <span>words</span></code></pre>
<p>To run a scanner, we just feed it the entire input as a <code>String</code>, which gets chopped into tokens using <code>words</code>. (Of course in some scenarios we might want to use <code>lines</code> instead of <code>words</code>, or even do more complex tokenization.)</p>
<p>Note since <code>Scanner</code> is just a type synonym for <code>State [String]</code>, it is automatically an instance of <code>Functor</code>, <code>Applicative</code>, and <code>Monad</code> (but not <code>Alternative</code>).</p>
<p>So let’s develop a little <code>Scanner</code> DSL. The most fundamental thing we can do is read the next token.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>str</span> <span style="color:red;">::</span> <span>Scanner</span> <span>String</span>
<span>str</span> <span style="color:red;">=</span> <span>get</span> <span>&gt;&gt;=</span> <span style="color:red;">\</span><span style="color:blue;font-weight:bold;">case</span> <span style="color:red;">{</span> <span>s</span><span>:</span><span>ss</span> <span style="color:red;">-&gt;</span> <span>put</span> <span>ss</span> <span>&gt;&gt;</span> <span>return</span> <span>s</span> <span style="color:red;">}</span></code></pre>
<p>(This uses <a href="https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#lambdacase">the <code>LambdaCase</code> extension</a>, though we could easily rewrite it without.) <code>str</code> gets the current list of tokens, puts it back without the first token, and returns the first token. Note that I purposely didn’t include a case for the empty list. You might think we want to include a case for the empty token list and have it return the empty string or something like that. But since the input will always be properly formatted, if this scenario ever happens it means my program has a bug—<em>e.g.</em> perhaps I misunderstood the description of the input format. In this scenario I <em>want</em> it to crash loudly, as soon as possible, rather than continuing on with some bogus data.</p>
<p>We can now add some scanners for reading specific token types other than <code>String</code>, simply by mapping the <code>read</code> function over the output of <code>str</code>:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>int</span> <span style="color:red;">::</span> <span>Scanner</span> <span>Int</span>
<span>int</span> <span style="color:red;">=</span> <span>read</span> <span>&lt;$&gt;</span> <span>str</span>

<span>integer</span> <span style="color:red;">::</span> <span>Scanner</span> <span>Integer</span>
<span>integer</span> <span style="color:red;">=</span> <span>read</span> <span>&lt;$&gt;</span> <span>str</span>

<span>double</span> <span style="color:red;">::</span> <span>Scanner</span> <span>Double</span>
<span>double</span> <span style="color:red;">=</span> <span>read</span> <span>&lt;$&gt;</span> <span>str</span></code></pre>
<p>Again, these will crash if they see a token in an unexpected format, and that is a very deliberate choice.</p>
<p>Now, as I explained earlier, a very common pattern is to have an integer $n$ followed by $n$ copies of something. So let’s make a combinator to encapsulate that pattern:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>numberOf</span> <span style="color:red;">::</span> <span>Scanner</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Scanner</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span>
<span>numberOf</span> <span>s</span> <span style="color:red;">=</span> <span>int</span> <span>&gt;&gt;=</span> <span>flip</span> <span>replicateM</span> <span>s</span></code></pre>
<p><code>numberOf s</code> expects to first see an <code>Int</code> value $n$, and then it runs the provided scanner $n$ times, returning a list of the results.</p>
<p>It’s also sometimes useful to have a way to repeat a <code>Scanner</code> some unknown number of times until encountering EOF (for example, the input for some problems doesn’t specify the number of test cases up front the way that Popular Vote does). This is similar to the <code>many</code> combinator from <code>Alternative</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>many</span> <span style="color:red;">::</span> <span>Scanner</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Scanner</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span>
<span>many</span> <span>s</span> <span style="color:red;">=</span> <span>get</span> <span>&gt;&gt;=</span> <span style="color:red;">\</span><span style="color:blue;font-weight:bold;">case</span> <span style="color:red;">{</span> <span>[]</span> <span style="color:red;">-&gt;</span> <span>return</span> <span>[]</span><span style="color:red;">;</span> <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>:</span><span style="color:red;">)</span> <span>&lt;$&gt;</span> <span>s</span> <span>&lt;*&gt;</span> <span>many</span> <span>s</span> <span style="color:red;">}</span></code></pre>
<p><code>many s</code> repeats the scanner <code>s</code> as many times as it can, returning a list of the results. In particular it first peeks at the current token list to see if it is empty. If so, it returns the empty list of results; if there are more tokens, it runs <code>s</code> once and then recursively calls <code>many s</code>, consing the results together.</p>
<p>Finally, it’s quite common to want to parse a specific small number of something, <em>e.g.</em> two double values representing a 2D coordinate pair. We could just write <code>replicateM 2 double</code>, but this is common enough that I find it helpful to define dedicated combinators with short names:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>two</span><span style="color:red;">,</span> <span>three</span><span style="color:red;">,</span> <span>four</span> <span style="color:red;">::</span> <span>Scanner</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Scanner</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span>
<span style="color:red;">[</span><span>two</span><span style="color:red;">,</span> <span>three</span><span style="color:red;">,</span> <span>four</span><span style="color:red;">]</span> <span style="color:red;">=</span> <span>map</span> <span>replicateM</span> <span style="color:red;">[</span><span class="hs-num">2</span><span style="color:red;">..</span><span class="hs-num">4</span><span style="color:red;">]</span></code></pre>
<p>The complete file can be <a href="https://github.com/byorgey/comprog-hs/blob/master/Scanner.hs">found on GitHub</a>. As I continue this series I’ll be putting more code into that repository. Note I do not intend to make this into a Hackage package, since that wouldn’t be useful: you can’t tell Kattis to go download a package from Hackage before running your submission. However, it is possible to submit multiple files at once, so you can include <code>Scanner.hs</code> in your submission and just <code>import Scanner</code> at the top of your main module.</p>
<h2 id="examples">Examples</h2>
<p>So what have we gained? Writing the parser for Popular Vote is now almost trivial:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">type</span> <span>Election</span> <span style="color:red;">=</span> <span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span>

<span>main</span> <span style="color:red;">=</span> <span>interact</span> <span>$</span> <span>runScanner</span> <span>elections</span> <span>&gt;&gt;&gt;</span> <span>...</span>

<span>elections</span> <span style="color:red;">::</span> <span>Scanner</span> <span style="color:red;">[</span><span>Election</span><span style="color:red;">]</span>
<span>elections</span> <span style="color:red;">=</span> <span>numberOf</span> <span style="color:red;">(</span><span>numberOf</span> <span>int</span><span style="color:red;">)</span></code></pre>
<p>In practice I would probably just inline the definition of <code>elections</code> directly: <code>interact $ runScanner (numberOf (numberOf int)) &gt;&gt;&gt; ...</code></p>
<p>As a slightly more involved example, chosen almost at random, consider <a href="https://open.kattis.com/problems/wrapping">Board Wrapping</a>:</p>
<blockquote>
On the first line of input there is one integer, $N \leq 50$, giving the number of test cases (moulds) in the input. After this line, $N$ test cases follow. Each test case starts with a line containing one integer $n, 1 \leq n \leq 600$, which is the number of boards in the mould. Then $n$ lines follow, each with five floating point numbers $x,y,w,h,v$ where $0 \leq x,y,w,h \leq 10000$ and $-90^{\circ} &lt; v \leq 90^{\circ}$. The $x$ and $y$ are the coordinates of the center of the board and $w$ and $h$ are the width and height of the board, respectively. $v$ is the angle between the height axis of the board to the $y$-axis in degrees, positive clockwise.
</blockquote>
<p>Here’s how I would set up the input, using <code>Scanner</code> and a custom data type to represent boards.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">import</span> <span>Scanner</span>

<span style="color:blue;font-weight:bold;">type</span> <span>V</span> <span style="color:red;">=</span> <span style="color:red;">[</span><span>Double</span><span style="color:red;">]</span>     <span style="color:green;">-- 2D vectors/points</span>
<span style="color:blue;font-weight:bold;">newtype</span> <span>A</span> <span style="color:red;">=</span> <span>A</span> <span>Double</span>  <span style="color:green;">-- angle (radians)</span>
                      <span style="color:green;">-- newtype helps avoid conversion errors</span>

<span>fromDeg</span> <span style="color:red;">::</span> <span>Double</span> <span style="color:red;">-&gt;</span> <span>A</span>
<span>fromDeg</span> <span>d</span> <span style="color:red;">=</span> <span>A</span> <span style="color:red;">(</span><span>d</span> <span>*</span> <span>pi</span> <span>/</span> <span class="hs-num">180</span><span style="color:red;">)</span>

<span style="color:blue;font-weight:bold;">data</span> <span>Board</span> <span style="color:red;">=</span> <span>Board</span> <span style="color:red;">{</span> <span>boardLoc</span> <span style="color:red;">::</span> <span>V</span><span style="color:red;">,</span> <span>boardDims</span> <span style="color:red;">::</span> <span>V</span><span style="color:red;">,</span> <span>boardAngle</span> <span style="color:red;">::</span> <span>A</span> <span style="color:red;">}</span>

<span>board</span> <span style="color:red;">::</span> <span>Scanner</span> <span>Board</span>
<span>board</span> <span style="color:red;">=</span> <span>Board</span>
  <span>&lt;$&gt;</span> <span>two</span> <span>double</span>
  <span>&lt;*&gt;</span> <span>two</span> <span>double</span>
  <span>&lt;*&gt;</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>fromDeg</span> <span>.</span> <span>negate</span><span style="color:red;">)</span> <span>&lt;$&gt;</span> <span>double</span><span style="color:red;">)</span>

<span>main</span> <span style="color:red;">=</span> <span>interact</span> <span>$</span>
  <span>runScanner</span> <span style="color:red;">(</span><span>numberOf</span> <span style="color:red;">(</span><span>numberOf</span> <span>board</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>...</span></code></pre>

