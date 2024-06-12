---
katex: true
title: 'Competitive programming in Haskell: introduction to dynamic programming'
published: 2023-05-31T14:47:46Z
categories: competitive programming,haskell
tags: challenge,dynamic programming,Kattis
---

<p>In <a href="https://byorgey.wordpress.com/2023/05/24/competitive-programming-in-haskell-parsing-with-an-nfa/">my previous post</a>, I challenged you to solve <a href="https://open.kattis.com/problems/zapis">Zapis</a>. In this problem, we are given a sequence of opening and closing brackets (parens, square brackets, and curly braces) with question marks, and have to compute the number of different ways in which the question marks could be replaced by brackets to create valid, properly nested bracket sequences.</p>
<p>For example, given <code>(??)</code>, the answer is 4: we could replace the question marks with any matched pair (either <code>()</code>, <code>[]</code>, or <code>{}</code>), or we could replace them with <code>)(</code>, resulting in <code>()()</code>.</p>
<h2 id="an-annoying-aside">An annoying aside</h2>
<p>One very annoying thing to mention about this problem is that it requires us to output the <em>last 5 digits</em> of the answer. At first, I interpreted that to mean “output the answer modulo $10^5$”, which would be a standard sort of condition for a combinatorics problem, but that’s not quite the same thing, in a very annoying way: for example, if the answer is $2$, we are supposed to output <code>2</code>; but if the answer is $1000000002$, we are supposed to output <code>00002</code>, not <code>2</code>! So simply computing the answer modulo $10^5$ is not good enough; if we get a final answer of $2$, we don’t know whether we are supposed to pad it with zeros. I could imagine keeping track of both the result modulo $10^5$ along with a Boolean flag telling us whether the number has ever overflowed; we have to pad with zeros iff the flag is set at the end. I’m pretty sure this would work. But for this problem, it turns out that the final answer is at most “only” about 100 digits, so we can just compute the answer exactly as an <code>Integer</code> and then literally show the last 5 digits.</p>
<h2 id="a-recurrence">A recurrence</h2>
<p>Now, how to compute the answer? For this kind of problem the first step is to come up with a recurrence. Let $s[0 \dots n-1]$ be the given string, and let $c(i,j)$ be the number of ways to turn the substring $s[i \dots j-1]$ into a properly nested sequence of brackets, so ultimately we want to compute the value of $c(0,n)$. (Note we make $c(i,j)$ correspond to the substring which includes $i$ but excludes $j$, which means, for example, that the length of the substring is $j-i$.) First, some base cases:</p>
<ul>
<li>$c(i,i) = 1$ since the empty string always counts as properly nested.</li>
<li>$c(i,j) = 0$ if $i$ and $j$ have different parity, since any properly nested string must have even length.</li>
</ul>
<p>Otherwise, $s[i]$ had better be an opening bracket of some kind, and we can try matching it with each of $s[i+1]$, $s[i+3]$, $s[i+5]$, …, $s[j-1]$. In general, matching $s[i]$ with $s[k]$ can be done in either $0$, $1$, or $3$ ways depending on whether they are proper opening and closing brackets and whether any question marks are involved; then we have $c(i+1,k)$ ways to make the substring between $s[i]$ and $s[k]$ properly nested, and $c(k+1,j)$ ways for the rest of the string following $s[k]$. These are all independent, so we multiply them. Overall, we get this:</p>
<p>$c(i,j) = \begin{cases} 1 & i = j \\ 0 & i \not \equiv j \pmod 2 \\ \displaystyle \sum_{k \in [i+1, i+3, \dots, j-1]} m(s[i], s[k]) \cdot c(i+1,k) \cdot c(k+1,j) & \text{otherwise} \end{cases}$</p>
<p>where $m(x,y)$ counts the number of ways to make $x$ and $y$ into a matching pair of brackets: it returns 0 if the two characters cannot possibly be a matching open-close pair (either because they do not match or because one of them is the wrong way around); 1 if they match, and at most one of them is a question mark; and 3 if both are question marks.</p>
<p>How do we come up with such recurrences in the first place? Unfortunately, Haskell doesn’t really make this any easier—it requires some experience and insight. However, what we can say is that Haskell makes it very easy to directly code a recurrence as a recursive function, to play with it and ensure that it gives correct results for small input values.</p>
<h2 id="a-naive-solution">A naive solution</h2>
<p>To that end, if we directly code up our recurrence in Haskell, we get the following naive solution:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color: blue;font-weight: bold">import</span> <span>Control.Arrow</span>
<span style="color: blue;font-weight: bold">import</span> <span>Data.Array</span>

<span>main</span> <span style="color: red">=</span> <span>interact</span> <span>$</span> <span>lines</span> <span>&gt;&gt;&gt;</span> <span>last</span> <span>&gt;&gt;&gt;</span> <span>solve</span> <span>&gt;&gt;&gt;</span> <span>format</span>

<span>format</span> <span style="color: red">::</span> <span>Integer</span> <span style="color: red">-&gt;</span> <span>String</span>
<span>format</span> <span style="color: red">=</span> <span>show</span> <span>&gt;&gt;&gt;</span> <span>reverse</span> <span>&gt;&gt;&gt;</span> <span>take</span> <span class="hs-num">5</span> <span>&gt;&gt;&gt;</span> <span>reverse</span>

<span>solve</span> <span style="color: red">::</span> <span>String</span> <span style="color: red">-&gt;</span> <span>Integer</span>
<span>solve</span> <span>str</span> <span style="color: red">=</span> <span>c</span> <span style="color: red">(</span><span class="hs-num">0</span><span style="color: red">,</span><span>n</span><span style="color: red">)</span>
  <span style="color: blue;font-weight: bold">where</span>
    <span>n</span> <span style="color: red">=</span> <span>length</span> <span>str</span>
    <span>s</span> <span style="color: red">=</span> <span>listArray</span> <span style="color: red">(</span><span class="hs-num">0</span><span style="color: red">,</span><span>n</span><span style="color: green">-</span><span class="hs-num">1</span><span style="color: red">)</span> <span>str</span>

    <span>c</span> <span style="color: red">::</span> <span style="color: red">(</span><span>Int</span><span style="color: red">,</span> <span>Int</span><span style="color: red">)</span> <span style="color: red">-&gt;</span> <span>Integer</span>
    <span>c</span> <span style="color: red">(</span><span>i</span><span style="color: red">,</span><span>j</span><span style="color: red">)</span>
      <span style="color: red">|</span> <span>i</span> <span>==</span> <span>j</span>           <span style="color: red">=</span> <span class="hs-num">1</span>
      <span style="color: red">|</span> <span>even</span> <span>i</span> <span>/=</span> <span>even</span> <span>j</span> <span style="color: red">=</span> <span class="hs-num">0</span>
      <span style="color: red">|</span> <span>otherwise</span>        <span style="color: red">=</span> <span>sum</span>
        <span style="color: red">[</span> <span>m</span> <span style="color: red">(</span><span>s</span><span>!</span><span>i</span><span style="color: red">)</span> <span style="color: red">(</span><span>s</span><span>!</span><span>k</span><span style="color: red">)</span> <span>*</span> <span>c</span> <span style="color: red">(</span><span>i</span><span>+</span><span class="hs-num">1</span><span style="color: red">,</span><span>k</span><span style="color: red">)</span> <span>*</span> <span>c</span> <span style="color: red">(</span><span>k</span><span>+</span><span class="hs-num">1</span><span style="color: red">,</span><span>j</span><span style="color: red">)</span>
        <span style="color: red">|</span> <span>k</span> <span style="color: red">&lt;-</span> <span style="color: red">[</span><span>i</span><span>+</span><span class="hs-num">1</span><span style="color: red">,</span> <span>i</span><span>+</span><span class="hs-num">3</span> <span style="color: red">..</span> <span>j</span><span style="color: green">-</span><span class="hs-num">1</span><span style="color: red">]</span>
        <span style="color: red">]</span>

<span>m</span> <span style="color: teal">'('</span> <span style="color: teal">')'</span>                <span style="color: red">=</span> <span class="hs-num">1</span>
<span>m</span> <span style="color: teal">'['</span> <span style="color: teal">']'</span>                <span style="color: red">=</span> <span class="hs-num">1</span>
<span>m</span> <span style="color: teal">'{'</span> <span style="color: teal">'}'</span>                <span style="color: red">=</span> <span class="hs-num">1</span>
<span>m</span> <span style="color: teal">'?'</span> <span style="color: teal">'?'</span>                <span style="color: red">=</span> <span class="hs-num">3</span>
<span>m</span> <span>b</span> <span style="color: teal">'?'</span> <span style="color: red">|</span> <span>b</span> <span>`elem`</span> <span style="color: teal">"([{"</span> <span style="color: red">=</span> <span class="hs-num">1</span>
<span>m</span> <span style="color: teal">'?'</span> <span>b</span> <span style="color: red">|</span> <span>b</span> <span>`elem`</span> <span style="color: teal">")]}"</span> <span style="color: red">=</span> <span class="hs-num">1</span>
<span>m</span> <span style="color: blue;font-weight: bold">_</span> <span style="color: blue;font-weight: bold">_</span>                    <span style="color: red">=</span> <span class="hs-num">0</span></code></pre>
<p>This solution is correct, but much too slow—it passes the first four test cases but then fails with a <em>Time Limit Exceeded</em> error. In fact, it takes exponential time in the length of the input string, because it has a classic case of <a href="https://en.wikipedia.org/wiki/Overlapping_subproblems">overlapping subproblems</a>. Our goal is to compute the same function, but in a way that is actually efficient.</p>
<h2 id="dynamic-programming-aka-memoizing-recurrences">Dynamic programming, aka memoizing recurrences</h2>
<p>I hate the name “dynamic programming”—it conveys zero information about the thing that it names, and was essentially <a href="https://www.linkedin.com/pulse/origins-name-dynamic-programming-ashwin-rao/">invented as a marketing gimmick</a>. Dynamic programming is really just memoizing recurrences in order to compute them more efficiently. By <em>memoizing</em> we mean caching some kind of mapping from input to output values, so that we only have to compute a function once for each given input value; on subsequent calls with a repeated input we can just look up the corresponding output. There are many, many variations on the theme, but memoizing recurrences is really the heart of it.</p>
<p>In imperative languages, dynamic programming is often carried out by filling in tables via nested loops—the fact that there is a <em>recurrence</em> involved is obscured by the implementation. However, in Haskell, our goal will be to write code that is <em>as close as possible</em> to the above naive recursive version, but still actually efficient. Over the next few posts we will discuss several techniques for doing just that.</p>
<ul>
<li>In <a href="https://byorgey.wordpress.com/2023/06/02/dynamic-programming-in-haskell-lazy-immutable-arrays/">part 1</a>, we will explore the basic idea of using lazy, recursive, immutable arrays (which we have <a href="https://byorgey.wordpress.com/2023/04/11/competitive-programming-in-haskell-topsort-via-laziness/">already seen in a previous post</a>).</li>
<li>In <a href="https://byorgey.wordpress.com/2023/06/06/dynamic-programming-in-haskell-automatic-memoization/">part 2</a>, we will use ideas from Conal Elliot’s <code>MemoTrie</code> package (and ultimately from <a href="https://citeseerx.ist.psu.edu/doc/10.1.1.43.3272">a paper by Ralf Hinze</a>) to clean up the code and make it a lot closer to the naive version.</li>
<li><a href="https://byorgey.wordpress.com/2023/06/20/competitive-programming-in-haskell-two-more-dp-challenges/">This post</a> contains a couple challenge problems that can’t quite be solved using the techniques in the previous posts.</li>
<li>At some point perhaps we’ll discuss how to memoize functions with infinite (or just very large) domains.</li>
<li>There may very well end up being more parts… we’ll see where it ends up!</li>
</ul>
<p>Along the way I’ll also drop more links to relevant background. This will ultimately end up as a chapter in the book I’m slowly writing, and I’d like to make it into the definitive reference on dynamic programming in Haskell—so any thoughts, comments, links, etc. are most welcome!</p>

