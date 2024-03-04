---
katex: true
title: 'Competitive Programming in Haskell: Basic Setup'
published: 2019-04-24T15:49:22Z
categories: haskell
tags: competitive,composition,IO,lazy,programming
---

<p>I am the <a href="https://byorgey.wordpress.com/2018/11/04/hendrix-teams-at-acm-icpc/">coach of my school’s competitive programming team</a> and <a href="https://open.kattis.com/users/byorgey">enjoy solving problems</a> on <a href="http://open.kattis.com">Open Kattis</a>. Since Kattis accepts submissions in a <a href="https://open.kattis.com/help">wide variety of languages</a> (including Haskell, OCaml, Rust, Common Lisp, and even Prolog), I often enjoy submitting solutions in Haskell. Of the 946 problems I have solved on Kattis<a href="#fn1" class="footnote-ref" id="fnref1"><sup>1</sup></a>, I used Haskell for 607 of them (I used Java for the rest, except for one in C++).</p>
<p>After solving so many problems in Haskell, by now I’ve figured out some patterns that work well, identified some common pitfalls, developed some nice little libraries, and so forth. I thought it would be fun to write a series of blog posts sharing my experience for the benefit of others—and because I expect I will also learn things from the ensuing discussion!</p>
<h2 id="the-basics-io">The Basics: I/O</h2>
<p>As a basic running example I’ll use the same example problem that Kattis uses in its <a href="https://open.kattis.com/help">help section</a>, namely, <a href="https://open.kattis.com/problems/different">A Different Problem</a>. In this problem, we are told that the input will consist of a number of pairs of integers between $0$ and $10^{15}$, one pair per line, and we should output the absolute value of the difference between each pair. The given example is that if the input looks like this:</p>
<pre><code>10 12
71293781758123 72784
1 12345677654321</code></pre>
<p>then our program should produce output that looks like this:</p>
<pre><code>2
71293781685339
12345677654320</code></pre>
<p>Kattis problems are always set up this way, with input of a specified format provided on standard input, and output to be written to standard output. To do this in Haskell, one might think we will need to use things like <code>getLine</code> and <code>putStrLn</code> to read and write the input. But wait! There is a much better way. Haskell’s standard <code>Prelude</code> has a function</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>interact</span> <span style="color:red;">::</span> <span style="color:red;">(</span><span>String</span> <span style="color:red;">-&gt;</span> <span>String</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>IO</span> <span>()</span></code></pre>
<p>It takes a pure <code>String -&gt; String</code> function, and creates an <code>IO</code> action which reads from standard input, feeds the input to the function, and then writes the function’s output to standard output. It uses lazy IO, so the reading and writing can be interleaved with computation of the function—a bit controversial and dangerous in general, but absolutely perfect for our use case! Every single Kattis problem I have ever solved begins with</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>main</span> <span style="color:red;">=</span> <span>interact</span> <span>$</span> <span>...</span></code></pre>
<p>(or the equivalent for <code>ByteString</code>, more on that in a future post) and that is the only bit of <code>IO</code> in the entire program. Yay!</p>
<h2 id="from-input-to-output">From Input to Output</h2>
<p>So now we need to write a pure function which transforms the input into the output. Of course, in true Haskell fashion, we will do this by constructing a chained pipeline of functions to do the job incrementally. The general plan of attack (for any Kattis problem) is as follows:</p>
<ol type="1">
<li>First, parse the input, that is, transform the raw <code>String</code> input into some more semantically meaningful representation—typically using a combination of functions like <code>lines</code>, <code>words</code>, <code>read</code>, <code>map</code>, and so on (or more sophisticated tools—see a later post).</li>
<li>Next, solve the problem, turning a semantically meaningful representation of the input into a semantically meaningful representation of the output.</li>
<li>Finally, format the output using things like <code>show</code>, <code>unwords</code>, <code>unlines</code>, and so on.</li>
</ol>
<p>Idiomatic Haskell uses the composition operator <code>(.)</code> to combine functions. However, when solving competitive programming problems, I much prefer to use the reverse composition operator, <code>(&gt;&gt;&gt;)</code> from <code>Control.Arrow</code> (that is, <code>(&gt;&gt;&gt;) = flip (.)</code>). The reason is that since I often end up constructing long function pipelines, I want to be able to think about the process of transforming input to output and type from left to right at the same time; having to add functions from right to left would be tedious.</p>
<h2 id="a-full-solution">A Full Solution</h2>
<p>So here’s my solution to <a href="https://open.kattis.com/problems/different">A Different Problem</a>:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>main</span> <span style="color:red;">=</span> <span>interact</span> <span>$</span>
  <span>lines</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>words</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span>read</span> <span>&gt;&gt;&gt;</span> <span>solve</span> <span>&gt;&gt;&gt;</span> <span>show</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>unlines</span>

<span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Integer</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Integer</span>
<span>solve</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">,</span><span>b</span><span style="color:red;">]</span> <span style="color:red;">=</span> <span>abs</span> <span style="color:red;">(</span><span>a</span> <span style="color:green;">-</span> <span>b</span><span style="color:red;">)</span></code></pre>
<p>A few notes:</p>
<ul>
<li>Since each line is to be processed independently, notice how I put the processing of each line inside a single call to <code>map</code>.</li>
<li>We could probably inline the <code>solve</code> function in this case, but I prefer to split it out explicitly in order to specify its type, which both prevents problems with <code>read</code>/<code>show</code> ambiguity and also serves as a sanity check on the parsing and formatting code.</li>
<li>The machines on which our solution will run definitely have 64-bit architectures, so we could technically get away with using <code>Int</code> instead of <code>Integer</code> (<code>maxBound :: Int64</code> is a bit more than $9 \times 10^{18}$, plenty big enough for inputs up to $10^{15}$), but there would be no benefit to doing so. If we use <code>Integer</code> we don’t even have to consider potential problems with overflow.</li>
</ul>
<p>And one last thing: I said we were going to parse the input into a “semantically meaningful representation”, but I lied a teensy bit: the problem says we are going to get a <em>pair</em> of integers but I wrote my <code>solve</code> function as though it takes a <em>list</em> of integers. And even worse, my <code>solve</code> function is partial! Why did I do that?</p>
<p>The fact is that I almost never use actual Haskell tuples in my solutions, because they are too awkward and inconvenient. Representing homogeneous tuples as Haskell lists of a certain known length allows us to read and process “tuples” using standard functions like <code>words</code> and <code>map</code>, to combine them using <code>zipWith</code>, and so on. And since we get to assume that the input always precisely follows the specification—which will never change—this is one of the few situations where, in my opinion, we are fully justified in writing partial functions like this if it makes the code easier to write. So I always represent homogeneous tuples as lists and just pattern match on lists of the appropriate (known) length. (If I need heterogeneous tuples, on the other hand, I create an appropriate <code>data</code> type.)</p>
<p>Of course I’ve only scratched the surface here—I’ll have a lot more to say in future posts—but this should be enough to get you started! I’ll leave you with a few very easy problems, which can each be done with just a few lines of Haskell:</p>
<ul>
<li><a href="https://open.kattis.com/problems/jobexpenses">Job Expenses</a></li>
<li><a href="http://open.kattis.com/problems/judgingmoose">Judging Moose</a></li>
<li><a href="http://open.kattis.com/problems/quickestimate">Quick Estimate</a></li>
</ul>
<p>Of course you can also try solving any of the other problems (as of this writing, over 2400 of them!) on Kattis as well.</p>
<section class="footnotes">
<hr />
<ol>
<li id="fn1"><p>Did I mention that I <em>really</em> enjoy solving competitive programming problems?<a href="#fnref1" class="footnote-back">↩</a></p></li>
</ol>
</section>

