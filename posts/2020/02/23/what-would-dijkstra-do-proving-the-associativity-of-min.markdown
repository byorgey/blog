---
katex: true
title: 'What would Dijkstra do? Proving the associativity of min'
published: 2020-02-23T04:07:43Z
categories: math
tags: associativity,calculation,Dijkstra,meet,min,poset,proof
---

<p>This semester I’m teaching a Discrete Mathematics course. Recently, I assigned them a homework problem from the textbook that asked them to prove that the binary $\min$ operator on the real numbers is associative, that is, for all real numbers $a$, $b$, and $c$,</p>
<div style="text-align:center;">
<p>$\min(a, \min(b,c)) = \min(\min(a,b), c)$.</p>
</div>
<p>You might like to pause for a minute to think about how you would prove this! Of course, how you prove it depends on how you define $\min$, so you might like to think about that too.</p>
<p>The book expected them to do a proof by cases, with some sort of case split on the order of $a$, $b$, and $c$. What they turned in was mostly pretty good, actually, but while grading it I became disgusted with the whole thing and thought there has to be a better way.</p>
<p>I was reminded of an example of Dijkstra’s that I remember reading. So I asked myself—what would Dijkstra do? The thing I remember reading may have, in fact, been this exact proof, but I couldn’t remember any details and I still can’t find it now, so I had to (re-)work out the details, guided only by some vague intuitions.</p>
<p>Dijkstra would certainly advocate proving associativity of $\min$ using a calculational approach. Dijkstra would also advocate using a symmetric infix operator symbol for a commutative and associative operation, so let’s adopt the symbol $\downarrow$ for $\min$. ($\sqcap$ would also be a reasonable choice, though I find it less mnemonic.)</p>
<p>How can we calculate with $\downarrow$? We have to come up with some way to characterize it that allows us to transform expressions involving $\downarrow$ into something else more fundamental. The most obvious definition would be “$a \downarrow b = a$ if $a \leq b$, and $b$ otherwise”. However, although this is a fantastic <em>implementation</em> of $\downarrow$ if you actually want to run it, it is not so great for reasoning about $\downarrow$, precisely because it involves doing a case split on whether $a \leq b$. This is the definition that leads to the ugly proof by cases.</p>
<p>How else could we define it? The usual more mathematically sophisticated way to define it would be as a greatest lower bound, that is, “$x = a \downarrow b$ if and only if $x \leq a$ and $x \leq b$ and $x$ is the greatest such number, that is, for any other $y$ such that $y \leq a$ and $y \leq b$, we have $y \leq x$.” However, this is a bit roundabout and also not so conducive to calculation.</p>
<p>My first epiphany was that the best way to characterize $\downarrow$ is by its relationship to $\leq$. After one or two abortive attempts, I hit upon the right idea:</p>
<p>$(a \leq b \downarrow c) \leftrightarrow (a \leq b \land a \leq c)$</p>
<p>That is, an arbitrary $a$ is less than or equal to the minimum of $b$ and $c$ precisely when it is less than or equal to both. In fact, this completely characterizes $\downarrow$, and is equivalent to the second definition given above.<a href="#fn1" class="footnote-ref" id="fnref1"><sup>1</sup></a> (You should try convincing yourself of this!)</p>
<p>But how do we get anywhere from $a \downarrow (b \downarrow c)$ by itself? We need to somehow introduce a thing which is less than or equal to it, so we can apply our characterization. My second epiphany was that equality of real numbers can also be characterized by having the same “downsets”, <em>i.e.</em> two real numbers are equal if and only if the sets of real numbers less than or equal to them are the same. That is,</p>
<p>$(x = y) \leftrightarrow (\forall z.\; (z \leq x) \leftrightarrow (z \leq y))$</p>
<p>Now the proof almost writes itself. Let $z \in \mathbb{R}$ be arbitrary; we calculate as follows:</p>
<p>$\begin{array}{cl} &amp; z \leq a \downarrow (b \downarrow c) \\ \leftrightarrow &amp; \\ &amp; z \leq a \land (z \leq b \downarrow c) \\ \leftrightarrow &amp; \\ &amp; z \leq a \land (z \leq b \land z \leq c) \\ \leftrightarrow &amp; \\ &amp; (z \leq a \land z \leq b) \land z \leq c \\ \leftrightarrow &amp; \\ &amp; (z \leq a \downarrow b) \land z \leq c \\ \leftrightarrow &amp; \\ &amp; z \leq (a \downarrow b) \downarrow c \end{array}$</p>
<p>Of course this uses our characterization of $\downarrow$ via its relationship to $\leq$, along with the fact that $\land$ is associative. Since we have proven that $z \leq a \downarrow (b \downarrow c)$ if and only if $z \leq (a \downarrow b) \downarrow c$ for arbitrary $z$, therefore $a \downarrow (b \downarrow c) = (a \downarrow b) \downarrow c$.</p>
<section class="footnotes">
<hr />
<ol>
<li id="fn1"><p>Thinking about it later, I realized that this should not be surprising: it’s just characterizing $\downarrow$ as the categorical product, <em>i.e.</em> meet, <em>i.e.</em> greatest lower bound, in the poset of real numbers ordered by the usual $\leq$.<a href="#fnref1" class="footnote-back">↩</a></p></li>
</ol>
</section>

