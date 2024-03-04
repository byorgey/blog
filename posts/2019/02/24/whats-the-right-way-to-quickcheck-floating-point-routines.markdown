---
katex: true
title: 'What’s the right way to QuickCheck floating-point routines?'
published: 2019-02-24T04:46:08Z
categories: haskell,math
tags: diagrams,finding,floating,point,polynomial,QuickCheck,root,solve,test
---

<p>I got a lot of great comments on <a href="https://byorgey.wordpress.com/2019/02/13/finding-roots-of-polynomials-in-haskell/">my previous post</a> about finding roots of polynomials in Haskell. One particularly promising idea I got from commenter Jake was to give up on the idea of having no external dependencies (which, to be fair, in these days of <code>stack</code> and <code>nix</code> and <code>cabal-v2</code>, seems like much less of a big deal than it used to be), and use the <code>hmatrix</code> package to find the eigenvalues of the <a href="https://en.wikipedia.org/wiki/Companion%20matrix">companion matrix</a>, which are exactly the roots.</p>
<p>So I tried that, and it seems to work great! The only problem is that I still don’t know how to write a reasonable test suite. I started by making a QuickCheck property expressing the fact that if we evaluate a polynomial at the returned roots, we should get something close to zero. I evaluate the polynomial using <a href="https://en.wikipedia.org/wiki/Horner%27s_method">Horner’s method</a>, which as far as I understand has good numerical stability in addition to being efficient.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>polyRoots</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Double</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Double</span><span style="color:red;">]</span>
<span>polyRoots</span> <span style="color:red;">=</span> <span>...</span> <span>stuff</span> <span>using</span> <span>hmatrix</span><span style="color:red;">,</span> <span>see</span> <span>code</span> <span>at</span> <span>end</span> <span style="color:blue;font-weight:bold;">of</span> <span>post</span> <span>...</span>

<span>horner</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Double</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Double</span> <span style="color:red;">-&gt;</span> <span>Double</span>
<span>horner</span> <span style="color:blue;font-weight:bold;">as</span> <span>x</span> <span style="color:red;">=</span> <span>foldl'</span> <span style="color:red;">(</span><span style="color:red;">\</span><span>r</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>r</span><span>*</span><span>x</span> <span>+</span> <span>a</span><span style="color:red;">)</span> <span class="hs-num">0</span> <span style="color:blue;font-weight:bold;">as</span>

<span class="hs-sel">_polyRoots_prop</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Double</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Property</span>
<span class="hs-sel">_polyRoots_prop</span> <span style="color:blue;font-weight:bold;">as</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>length</span> <span style="color:blue;font-weight:bold;">as</span> <span>&gt;</span> <span class="hs-num">1</span><span style="color:red;">)</span> <span>==&gt;</span>
  <span>all</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>&lt;</span> <span class="hs-num">1e-10</span><span style="color:red;">)</span> <span>.</span> <span>abs</span> <span>.</span> <span>horner</span> <span style="color:blue;font-weight:bold;">as</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>polyRoots</span> <span style="color:blue;font-weight:bold;">as</span><span style="color:red;">)</span></code></pre>
<p>This property passes 100 tests for quadratic polynomials, but for cubic I get failures; here’s an example. Consider the polynomial</p>
<p>$0.1 x^3 - 15.005674483568866 x^2 - 8.597718287916894 x + 8.29$</p>
<p>Finding its roots via <code>hmatrix</code> yields three:</p>
<p><code>[-1.077801388041068, 0.5106483227001805, 150.6238979010295]</code></p>
<p>Notice that the third root is much bigger in magnitude than the other two, and indeed, that third root is the problematic one. Evaluating the polynomial at these roots via Horner’s method yields</p>
<p><code>[1.2434497875801753e-14, 1.7763568394002505e-15, -1.1008971512183052e-10]</code></p>
<p>the third of which is bigger than <code>1e-10</code> which I had (very arbitrarily!) chosen as the cutoff for “close enough to zero”. But here’s the thing: after playing around with it a bit, it seems like this is the <em>most accurate possible value</em> for the root that can be represented using <code>Double</code>. That is, if I evaluate the polynomial at any value other than the root that was returned—even if I just change the very last digit by 1 in either direction—I get a result which is <em>farther</em> from zero.</p>
<p>If I make the magic cutoff value bigger—say, <code>1e-8</code> instead of <code>1e-10</code>—then I still get similar counterexamples, but for larger-degree polynomials. I never liked the arbitrary choice of a tolerance anyway, and now it seems to me that saying “evaluating the polynomial at the computed roots should be within this absolute distance from zero” is fundamentally the wrong thing to say; depending on the polynomial, we might have to take what we can get. Some other things I could imagine saying instead include:</p>
<ul>
<li>Evaluating the polynomial at the computed roots should be within some <em>relative</em> epsilon of zero, depending on the size/relative size of the coefficients</li>
<li>The computed roots are as accurate as possible (or close to it) in the sense that evaluating the polynomial at other numbers near the computed roots yields values which are farther from zero</li>
</ul>
<p>…but, first of all, I don’t know if these are reasonable properties to expect; and even if they were, I’m not sure I know how to express them in Haskell! Any advice is most welcome. Are there any best practices for expressing desirable test properties for floating-point computations?</p>
<p>For completeness, here is the actual code I came up with for finding roots via <code>hmatrix</code>. Notice there is another annoying arbitrary value in there, for deciding when a complex root is close enough to being real that we call it a real root. I’m not really sure what to do about this either.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- Compute the roots of a polynomial as the eigenvalues of its companion matrix,</span>
<span>polyRoots</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Double</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Double</span><span style="color:red;">]</span>
<span>polyRoots</span> <span>[]</span>     <span style="color:red;">=</span> <span>[]</span>
<span>polyRoots</span> <span style="color:red;">(</span><span class="hs-num">0</span><span>:</span><span style="color:blue;font-weight:bold;">as</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>polyRoots</span> <span style="color:blue;font-weight:bold;">as</span>
<span>polyRoots</span> <span style="color:red;">(</span><span>a</span><span>:</span><span style="color:blue;font-weight:bold;">as</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>mapMaybe</span> <span>toReal</span> <span>eigVals</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>n</span>   <span style="color:red;">=</span> <span>length</span> <span>as'</span>
    <span>as'</span> <span style="color:red;">=</span> <span>map</span> <span style="color:red;">(</span><span>/</span><span>a</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">as</span>
    <span>companion</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>konst</span> <span class="hs-num">0</span> <span style="color:red;">(</span><span class="hs-num">1</span><span style="color:red;">,</span><span>n</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span> <span>===</span> <span>ident</span> <span style="color:red;">(</span><span>n</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>|||</span> <span>col</span> <span style="color:red;">(</span><span>map</span> <span>negate</span> <span>.</span> <span>reverse</span> <span>$</span> <span>as'</span><span style="color:red;">)</span>

    <span>eigVals</span> <span style="color:red;">=</span> <span>toList</span> <span>.</span> <span>fst</span> <span>.</span> <span>eig</span> <span>$</span> <span>companion</span>
    <span>toReal</span> <span style="color:red;">(</span><span>a</span> <span>:+</span> <span>b</span><span style="color:red;">)</span>
       <span style="color:red;">|</span> <span>abs</span> <span>b</span> <span>&lt;</span> <span class="hs-num">1e-10</span> <span style="color:red;">=</span> <span>Just</span> <span>a</span>   <span style="color:green;">-- This arbitrary value is annoying too!</span>
       <span style="color:red;">|</span> <span>otherwise</span>     <span style="color:red;">=</span> <span>Nothing</span>
</code></pre>

