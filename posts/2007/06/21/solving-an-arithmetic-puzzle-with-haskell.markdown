---
title: Solving an arithmetic puzzle with Haskell
published: 2007-06-21T15:29:48Z
categories: haskell,math,puzzle
tags: 
---

[<strong>EDIT:</strong> since this post still seems to get a good deal of traffic, I should note that (as you can see if you read the comments) the code I gave here is not quite correct.  Still, it's interesting enough that I'll leave it up.]

<a href="http://jd2718.wordpress.com/">JD2718</a> posted a <a href="http://jd2718.wordpress.com/2007/06/07/4-3-2-1-and-maybe/">puzzle</a> the other day: the idea is to count how many possible results you can get by combining the numbers 4,3,2,1 (in that order) with the four arithmetic operators and parentheses.  Naturally I decided to write some Haskell code to solve this one.

First, instead of thinking in terms of possibly using parentheses, I just generate all possible postfix expressions.  But instead of creating some sort of algebraic data type to represent expressions, generating all possible expressions, and evaluating, I sort of interleaved the processes. First, I generate the list 4,3,2,1 with operations optionally applied along the way in all possible ways, then reduce them in all possible ways to a single result, discarding duplicates the whole time.  Here's the code:
<pre><font color="Green"><u>import</u></font> Data<font color="Cyan">.</font>Ratio
<font color="Green"><u>import</u></font> Data<font color="Cyan">.</font>List

<font color="Green"><u>type</u></font> Op <font color="Red">=</font> <font color="Cyan">(</font>Rational <font color="Red">-&gt;</font> Rational <font color="Red">-&gt;</font> Rational<font color="Cyan">)</font>
<font color="Green"><u>type</u></font> Stack <font color="Red">=</font> <font color="Red">[</font>Rational<font color="Red">]</font>

<font color="Blue">-- make a special kind of division to ignore division by zero. This</font>
<font color="Blue">-- doesn't give any spurious results since if we can get zero as one</font>
<font color="Blue">-- of the arguments, we can legitimately create another zero by</font>
<font color="Blue">-- multiplying.</font>
<font color="Cyan">(</font><font color="Cyan">//</font><font color="Cyan">)</font> <font color="Red">::</font> Rational <font color="Red">-&gt;</font> Rational <font color="Red">-&gt;</font> Rational
a <font color="Cyan">//</font> <font color="Magenta">0</font> <font color="Red">=</font> <font color="Magenta">0</font>
a <font color="Cyan">//</font> b <font color="Red">=</font> a <font color="Cyan">/</font> b

<font color="Blue">-- turn a normal binary operator into a function which operates </font>
<font color="Blue">-- on the top two elements of a stack.</font>
mkStackOp <font color="Red">::</font> Op <font color="Red">-&gt;</font> Stack <font color="Red">-&gt;</font> Stack
mkStackOp op <font color="Cyan">(</font>x1<font color="Red"><strong>:</strong></font>x2<font color="Red"><strong>:</strong></font>xs<font color="Cyan">)</font> <font color="Red">=</font> <font color="Cyan">(</font>x2 <font color="Cyan">`op`</font> x1<font color="Cyan">)</font> <font color="Red"><strong>:</strong></font> xs
mkStackOp op s <font color="Red">=</font> s

negateTop <font color="Red">::</font> Stack <font color="Red">-&gt;</font> Stack
negateTop <font color="Cyan">(</font>x<font color="Red"><strong>:</strong></font>xs<font color="Cyan">)</font> <font color="Red">=</font> <font color="Cyan">(</font>negate x<font color="Cyan">)</font> <font color="Red"><strong>:</strong></font> xs

<font color="Blue">-- operations that reduce a stack (i.e. binary operations)</font>
stackReducers <font color="Red">::</font> <font color="Red">[</font>Stack <font color="Red">-&gt;</font> Stack<font color="Red">]</font>
stackReducers <font color="Red">=</font> map mkStackOp <font color="Red">[</font><font color="Cyan">(</font><font color="Cyan">+</font><font color="Cyan">)</font><font color="Cyan">,</font> <font color="Cyan">(</font><font color="Blue">-</font><font color="Cyan">)</font><font color="Cyan">,</font> <font color="Cyan">(</font><font color="Cyan">*</font><font color="Cyan">)</font><font color="Cyan">,</font> <font color="Cyan">(</font><font color="Cyan">//</font><font color="Cyan">)</font><font color="Red">]</font>

<font color="Blue">-- operations that transform a stack without reducing it (unary</font>
<font color="Blue">-- operations).  to allow unary negation, just add negateTop to the</font>
<font color="Blue">-- list.</font>
stackTransformers <font color="Red">::</font> <font color="Red">[</font>Stack <font color="Red">-&gt;</font> Stack<font color="Red">]</font>
stackTransformers <font color="Red">=</font> <font color="Red">[</font>id<font color="Red">]</font>

allStackOps <font color="Red">=</font> stackReducers <font color="Cyan">++</font> stackTransformers

<font color="Blue">-- build up a stack by adding one more element (applying all possible</font>
<font color="Blue">-- stack transformers), while applying all possible operations to the</font>
<font color="Blue">-- previous elements.</font>
build <font color="Red">::</font> Rational <font color="Red">-&gt;</font> Stack <font color="Red">-&gt;</font> <font color="Red">[</font>Stack<font color="Red">]</font>
build n []  <font color="Red">=</font>     <font color="Red">[</font> f <font color="Red">[</font>n<font color="Red">]</font>             <font color="Red">|</font> f  <font color="Red">&lt;-</font> stackTransformers <font color="Red">]</font>
build n stk <font color="Red">=</font> nub <font color="Red">[</font> f <font color="Red">[</font>n<font color="Red">]</font> <font color="Cyan">++</font> <font color="Cyan">(</font>f' stk<font color="Cyan">)</font> <font color="Red">|</font> f  <font color="Red">&lt;-</font> stackTransformers<font color="Cyan">,</font>
                                        f' <font color="Red">&lt;-</font> allStackOps       <font color="Red">]</font>

<font color="Blue">-- perform one reduction on a stack in all possible ways.</font>
reduce1 <font color="Red">::</font> Stack <font color="Red">-&gt;</font> <font color="Red">[</font>Stack<font color="Red">]</font>
reduce1 <font color="Red">[</font>x<font color="Red">]</font> <font color="Red">=</font> <font color="Red">[</font><font color="Red">[</font>x<font color="Red">]</font><font color="Red">]</font>
reduce1 stk <font color="Red">=</font> <font color="Red">[</font> f stk <font color="Red">|</font> f <font color="Red">&lt;-</font> stackReducers <font color="Red">]</font>

<font color="Blue">-- like &gt;&gt;=, but discarding duplicates.  Ideally we would</font>
<font color="Blue">--   do this with a Monad instance of Data.Set, but that's</font>
<font color="Blue">--   currently not possible without doing some contortions</font>
<font color="Blue">--   to redefine the Monad class (since currently there's no</font>
<font color="Blue">--   way to define a monad over a subcategory of Haskell types,</font>
<font color="Blue">--   like we would need to define a Data.Set monad over only</font>
<font color="Blue">--   Eq types).  See http://www.randomhacks.net/articles/</font>
<font color="Blue">--   2007/03/15/data-set-monad-haskell-macros.</font>
l <font color="Cyan">&gt;&gt;-</font> f <font color="Red">=</font> nub <font color="Cyan">$</font> concatMap f l

<font color="Blue">-- completely reduce a stack to a single number in all possible ways.</font>
reduce <font color="Red">::</font> Stack <font color="Red">-&gt;</font> <font color="Red">[</font>Stack<font color="Red">]</font>
reduce <font color="Red">[</font>x<font color="Red">]</font> <font color="Red">=</font> <font color="Red">[</font><font color="Red">[</font>x<font color="Red">]</font><font color="Red">]</font>
reduce stk <font color="Red">=</font> reduce1 stk <font color="Cyan">&gt;&gt;-</font> reduce

<font color="Blue">-- build up stacks with the given rationals, then reduce.</font>
buildAndReduce <font color="Red">::</font> <font color="Red">[</font>Rational<font color="Red">]</font> <font color="Red">-&gt;</font> Stack <font color="Red">-&gt;</font> <font color="Red">[</font>Stack<font color="Red">]</font>
buildAndReduce [] <font color="Red">=</font> reduce
buildAndReduce <font color="Cyan">(</font>r<font color="Red"><strong>:</strong></font>rs<font color="Cyan">)</font> <font color="Red">=</font> s <font color="Red">-&gt;</font> <font color="Cyan">(</font>build r s <font color="Cyan">&gt;&gt;-</font> buildAndReduce rs<font color="Cyan">)</font>

<font color="Blue">-- given a list of starting numbers, return the list of all possible</font>
<font color="Blue">-- results using arithmetic operators and parentheses on the numbers</font>
<font color="Blue">-- in the given order.</font>
results <font color="Red">::</font> <font color="Red">[</font>Rational<font color="Red">]</font> <font color="Red">-&gt;</font> <font color="Red">[</font>Rational<font color="Red">]</font>
results rs <font color="Red">=</font> sort <font color="Cyan">$</font> concat <font color="Cyan">$</font> buildAndReduce rs []</pre>
There's a little ambiguity in the description of the problem: are we allowed to use - as prefix negation, or just as binary subtraction?  The code above treats it only as binary subtraction.  In that case we get 52 distinct results ranging from -5 = 4 - 3 * (2+1) to 36 = 4 * 3 * (2+1).  8 are negative, 28 are integers:
<pre>
Prelude&gt; :l fours
[1 of 1] Compiling Main             ( fours.hs, interpreted )
Ok, modules loaded: Main.
*Main&gt; results [4,3,2,1]
[(-5)%1,(-3)%1,(-2)%1,(-5)%3,(-1)%1,(-2)%3,(-1)%2,(-1)%3,
0%1,1%3,4%9,1%2,4%7,2%3,4%5,1%1,4%3,3%2,8%5,5%3,
2%1,7%3,5%2,8%3,3%1,10%3,7%2,11%3,4%1,13%3,9%2,
5%1,11%2,6%1,13%2,7%1,8%1,9%1,10%1,11%1,12%1,13%1,
14%1,15%1,16%1,20%1,21%1,23%1,24%1,25%1,28%1,36%1]
*Main&gt; length it
52
*Main&gt; length $ filter ((1==) . denominator) $ results [4,3,2,1]
28
*Main&gt; length $ filter (&lt;0) $ results [4,3,2,1]
8</pre>
If we allow unary negation, then we get 87 distinct possibilities, 47 of which are integers (and, of course, exactly half of the nonzero possibilities are negative, since they are just the negations of the positive possibilities).

