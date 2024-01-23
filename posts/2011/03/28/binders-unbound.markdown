---
title: 'Binders Unbound'
published: 2011-03-29T03:06:24Z
categories: grad school,haskell,projects,writing
tags: binding,EDSL,ICFP,library,names,paper
---

<p><a href="http://www.cis.upenn.edu/~sweirich/">Stephanie Weirich</a>, <a href="http://web.cecs.pdx.edu/~sheard/">Tim Sheard</a> and I recently submitted a paper to ICFP entitled <em>Binders Unbound</em>. (You can <a href="http://www.cis.upenn.edu/~byorgey/papers/binders-unbound.pdf">read a draft here</a>.) It's about our <strike>kick-ass</strike>, I mean, expressive and flexible library, <a href="http://hackage.haskell.org/package/unbound">unbound</a> (note: GHC 7 required), for generically dealing with names and binding structure when writing programs (compilers, interpreters, refactorers, proof assistants...) that work with syntax. Let's look at a small example of representing untyped lambda calculus terms. This post is working Literate Haskell, feel free to save it to a <code>.lhs</code> file and play around with it yourself!</p><p>First, we need to enable lots of wonderful GHC extensions:</p><pre><code><span>&gt;</span> <span style="color:green;">{-# LANGUAGE MultiParamTypeClasses
&gt;            , TemplateHaskell
&gt;            , ScopedTypeVariables
&gt;            , FlexibleInstances
&gt;            , FlexibleContexts
&gt;            , UndecidableInstances
&gt;   #-}</span>
</code></pre><p>Now to import the library and a few other things we'll need:</p><pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Unbound</span><span>.</span><span>LocallyNameless</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Control</span><span>.</span><span>Applicative</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Control</span><span>.</span><span>Arrow</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>+++</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Control</span><span>.</span><span>Monad</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Control</span><span>.</span><span>Monad</span><span>.</span><span>Trans</span><span>.</span><span>Maybe</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Text</span><span>.</span><span>Parsec</span> <span>hiding</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>&lt;|&gt;</span><span style="color:red;">)</span><span style="color:red;">,</span> <span>Empty</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Text</span><span>.</span><span>Parsec</span><span>.</span><span>Token</span> <span style="color:blue;font-weight:bold;">as</span> <span>P</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Text</span><span>.</span><span>Parsec</span><span>.</span><span>Language</span> <span style="color:red;">(</span><span>haskellDef</span><span style="color:red;">)</span>
</code></pre><p>We now declare a <code>Term</code> data type to represent lambda calculus terms.</p><pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Term</span> <span style="color:red;">=</span> <span>Var</span> <span style="color:red;">(</span><span>Name</span> <span>Term</span><span style="color:red;">)</span>
<span>&gt;</span>           <span style="color:red;">|</span> <span>App</span> <span>Term</span> <span>Term</span>
<span>&gt;</span>           <span style="color:red;">|</span> <span>Lam</span> <span style="color:red;">(</span><span>Bind</span> <span style="color:red;">(</span><span>Name</span> <span>Term</span><span style="color:red;">)</span> <span>Term</span><span style="color:red;">)</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span>Show</span>
</code></pre><p>The <code>App</code> constructor is straightforward, but the other two constructors are worth discussing in more detail. First, the <code>Var</code> constructor holds a <code>Name Term</code>. <code>Name</code> is an abstract type for representing names, provided by Unbound. <code>Name</code>s are indexed by the sorts of things to which they can refer (or more precisely, the sorts of things which can be substituted for them). Here, a variable is simply a name for some <code>Term</code>, so we use the type <code>Name Term</code>.</p><p>Lambdas are where names are <em>bound</em>, so we use the special <code>Bind</code> combinator, also provided by the library. Something of type <code>Bind p b</code> represents a pair consisting of a <em>pattern</em> <code>p</code> and a <em>body</em> <code>b</code>. The pattern may bind names which occur in <code>b</code>. Here is where the power of generic programming comes into play: we may use (almost) any types at all as patterns and bodies, and Unbound will be able to handle it with very little extra guidance from us. In this particular case, a lambda simply binds a single name, so the pattern is just a <code>Name Term</code>, and the body is just another <code>Term</code>.</p><p>Now we use Template Haskell to automatically derive a generic representation for <code>Term</code>:</p><pre><code><span>&gt;</span> <span>$</span><span style="color:red;">(</span><span>derive</span> <span style="color:red;">[</span><span style="color:teal;">'</span><span style="color:teal;">'</span><span>Term</span><span style="color:red;">]</span><span style="color:red;">)</span>
</code></pre><p>There are just a couple more things we need to do. First, we make <code>Term</code> an instance of <code>Alpha</code>, which provides most of the methods we will need for working with the variables and binders within <code>Term</code>s.</p><pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Alpha</span> <span>Term</span>
</code></pre><p>What, no method definitions? Nope! In this case (and in most cases) the default implementations, implemented in terms of automatically-derived generic representations, work just fine.</p><p>We also need to provide a <code>Subst Term Term</code> instance. In general, an instance for <code>Subst b a</code> means that we can use the <code>subst</code> function to substitute things of type <code>b</code> for <code>Name</code>s occurring in things of type <code>a</code>. We override the <code>isvar</code> method so the library knows which constructor(s) of our type represent variables which can be substituted for.</p><pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Subst</span> <span>Term</span> <span>Term</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>isvar</span> <span style="color:red;">(</span><span>Var</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>Just</span> <span style="color:red;">(</span><span>SubstName</span> <span>v</span><span style="color:red;">)</span>
<span>&gt;</span>   <span>isvar</span> <span style="color:blue;font-weight:bold;">_</span>       <span style="color:red;">=</span> <span>Nothing</span>
</code></pre><p>OK, now that we've got the necessary preliminaries set up, what can we do with this? Here's a little lambda-calculus evaluator:</p><pre><code><span>&gt;</span> <span>done</span> <span style="color:red;">::</span> <span>MonadPlus</span> <span>m</span> <span style="color:red;">=&gt;</span> <span>m</span> <span>a</span>
<span>&gt;</span> <span>done</span> <span style="color:red;">=</span> <span>mzero</span>
<span>&gt;</span> 
<span>&gt;</span> <span>step</span> <span style="color:red;">::</span> <span>Term</span> <span style="color:red;">-&gt;</span> <span>MaybeT</span> <span>FreshM</span> <span>Term</span>
<span>&gt;</span> <span>step</span> <span style="color:red;">(</span><span>Var</span> <span style="color:blue;font-weight:bold;">_</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>done</span>
<span>&gt;</span> <span>step</span> <span style="color:red;">(</span><span>Lam</span> <span style="color:blue;font-weight:bold;">_</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>done</span>
<span>&gt;</span> <span>step</span> <span style="color:red;">(</span><span>App</span> <span style="color:red;">(</span><span>Lam</span> <span>b</span><span style="color:red;">)</span> <span>t2</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span style="color:red;">(</span><span>x</span><span style="color:red;">,</span><span>t1</span><span style="color:red;">)</span> <span style="color:red;">&lt;-</span> <span>unbind</span> <span>b</span>
<span>&gt;</span>   <span>return</span> <span>$</span> <span>subst</span> <span>x</span> <span>t2</span> <span>t1</span>
<span>&gt;</span> <span>step</span> <span style="color:red;">(</span><span>App</span> <span>t1</span> <span>t2</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>       <span>App</span> <span>&lt;$&gt;</span> <span>step</span> <span>t1</span> <span>&lt;*&gt;</span> <span>pure</span> <span>t2</span>
<span>&gt;</span>   <span>&lt;|&gt;</span> <span>App</span> <span>&lt;$&gt;</span> <span>pure</span> <span>t1</span> <span>&lt;*&gt;</span> <span>step</span> <span>t2</span>
<span>&gt;</span> 
<span>&gt;</span> <span>tc</span> <span style="color:red;">::</span> <span style="color:red;">(</span><span>Monad</span> <span>m</span><span style="color:red;">,</span> <span>Functor</span> <span>m</span><span style="color:red;">)</span> <span style="color:red;">=&gt;</span> <span style="color:red;">(</span><span>a</span> <span style="color:red;">-&gt;</span> <span>MaybeT</span> <span>m</span> <span>a</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>a</span> <span style="color:red;">-&gt;</span> <span>m</span> <span>a</span><span style="color:red;">)</span>
<span>&gt;</span> <span>tc</span> <span>f</span> <span>a</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>ma'</span> <span style="color:red;">&lt;-</span> <span>runMaybeT</span> <span style="color:red;">(</span><span>f</span> <span>a</span><span style="color:red;">)</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">case</span> <span>ma'</span> <span style="color:blue;font-weight:bold;">of</span>
<span>&gt;</span>     <span>Just</span> <span>a'</span> <span style="color:red;">-&gt;</span> <span>tc</span> <span>f</span> <span>a'</span>
<span>&gt;</span>     <span>Nothing</span> <span style="color:red;">-&gt;</span> <span>return</span> <span>a</span>
<span>&gt;</span> 
<span>&gt;</span> <span>eval</span> <span style="color:red;">::</span> <span>Term</span> <span style="color:red;">-&gt;</span> <span>Term</span>
<span>&gt;</span> <span>eval</span> <span>x</span> <span style="color:red;">=</span> <span>runFreshM</span> <span style="color:red;">(</span><span>tc</span> <span>step</span> <span>x</span><span style="color:red;">)</span>
</code></pre><p>Note how we use <code>unbind</code> to take bindings apart safely, using the the <code>FreshM</code> monad (also provided by Unbound) for generating fresh names. We also get to use <code>subst</code> for capture-avoiding substitution. All without ever having to touch a de Bruijn index!</p><p>OK, but does it work? First, a little <a href="http://hackage.haskell.org/package/parsec">Parsec</a> parser:</p><pre><code><span>&gt;</span> <span>lam</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>Term</span> <span style="color:red;">-&gt;</span> <span>Term</span>
<span>&gt;</span> <span>lam</span> <span>x</span> <span>t</span> <span style="color:red;">=</span> <span>Lam</span> <span>$</span> <span>bind</span> <span style="color:red;">(</span><span>string2Name</span> <span>x</span><span style="color:red;">)</span> <span>t</span>
<span>&gt;</span> 
<span>&gt;</span> <span>var</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>Term</span>
<span>&gt;</span> <span>var</span> <span style="color:red;">=</span> <span>Var</span> <span>.</span> <span>string2Name</span>
<span>&gt;</span> 
<span>&gt;</span> <span>lexer</span>    <span style="color:red;">=</span> <span>P</span><span>.</span><span>makeTokenParser</span> <span>haskellDef</span>
<span>&gt;</span> <span>parens</span>   <span style="color:red;">=</span> <span>P</span><span>.</span><span>parens</span> <span>lexer</span>
<span>&gt;</span> <span>brackets</span> <span style="color:red;">=</span> <span>P</span><span>.</span><span>brackets</span> <span>lexer</span>
<span>&gt;</span> <span>ident</span>    <span style="color:red;">=</span> <span>P</span><span>.</span><span>identifier</span> <span>lexer</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parseTerm</span> <span style="color:red;">=</span> <span>parseAtom</span> <span>`chainl1`</span> <span style="color:red;">(</span><span>pure</span> <span>App</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parseAtom</span> <span style="color:red;">=</span> <span>parens</span> <span>parseTerm</span>
<span>&gt;</span>         <span>&lt;|&gt;</span> <span>var</span> <span>&lt;$&gt;</span> <span>ident</span>
<span>&gt;</span>         <span>&lt;|&gt;</span> <span>lam</span> <span>&lt;$&gt;</span> <span style="color:red;">(</span><span>brackets</span> <span>ident</span><span style="color:red;">)</span> <span>&lt;*&gt;</span> <span>parseTerm</span>
<span>&gt;</span> 
<span>&gt;</span> <span>runTerm</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>Either</span> <span>ParseError</span> <span>Term</span>
<span>&gt;</span> <span>runTerm</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>id</span> <span>+++</span> <span>eval</span><span style="color:red;">)</span> <span>.</span> <span>parse</span> <span>parseTerm</span> <span style="color:teal;">""</span>
</code></pre><p>Now let's try some <a href="http://en.wikipedia.org/wiki/Church_encoding">arithmetic</a>:</p><pre><code>*Main&gt; runTerm "([m][n][s][z] m s (n s z)) 
                ([s] [z] s (s z)) 
                ([s] [z] s (s (s z))) 
                s z"
Right (App (Var s) (App (Var s) (App (Var s) 
        (App (Var s) (App (Var s) (Var z))))))</code></pre><p>2 + 3 is still 5, and all is right with the world.</p><p>This blog post has only scratched the surface of what's possible. There are several other combinators other than just <code>Bind</code> for expressing binding structure: for example, nested bindings, recursive bindings and embedding terms within patterns are all supported. There are also other operations provided, such as free variable calculation, simultaneous unbinding, and name permutation. To learn more, <a href="http://hackage.haskell.org/package/unbound">see the package documentation</a>, or <a href="http://www.cis.upenn.edu/~byorgey/papers/binders-unbound.pdf">read the paper</a>!</p>

