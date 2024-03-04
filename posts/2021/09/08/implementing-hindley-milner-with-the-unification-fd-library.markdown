---
katex: true
title: 'Implementing Hindley-Milner with the unification-fd library'
published: 2021-09-08T17:31:29Z
categories: haskell,teaching
tags: Hindley-Milner,inference,types,unification
---

<p>For a current project, I needed to implement type inference for a <a href="https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system">Hindley-Milner</a>-based type system. (More about that project in an upcoming post!) If you don’t know, Hindley-Milner is what you get when you add polymorphism to the simply typed lambda calculus, but only allow $\forall$ to show up at the very outermost layer of a type. This is the fundamental basis for many real-world type systems (<em>e.g.</em> OCaml or Haskell without <code>RankNTypes</code> enabled).</p>
<p>One of the core operations in any Hindley-Milner type inference algorithm is <em>unification</em>, where we take two types that might contain <em>unification variables</em> (think “named holes”) and try to make them equal, which might fail, or might provide more information about the values that the unification variables should take. For example, if we try to unify <code>a -&gt; Int</code> and <code>Char -&gt; b</code>, we will learn that <code>a = Char</code> and <code>b = Int</code>; on the other hand, trying to unify <code>a -&gt; Int</code> and <code>(b, Char)</code> will fail, because there is no way to make those two types equal (the first is a function type whereas the second is a pair).</p>
<p>I’ve implemented this from scratch before, and it was a great learning experience, but I wasn’t looking forward to implementing it again. But then I remembered the <a href="https://hackage.haskell.org/package/unification-fd"><code>unification-fd</code> library</a> and wondered whether I could use it to simplify the implementation. Long story short, although the documentation for <code>unification-fd</code> claims it can be used to implement Hindley-Milner, I couldn’t find any examples online, and apparently <a href="https://github.com/wrengr/unification-fd/issues/17">neither could anyone else</a>. So I set out to make my own example, and you’re reading it. It turns out that <code>unification-fd</code> is incredibly powerful, but using it can be a bit finicky, so I hope this example can be helpful to others who wish to use it. Along the way, resources I found especially helpful include <a href="https://winterkoninkje.dreamwidth.org/100478.html">this basic unification-fd tutorial</a> by the author, Wren Romano, as well as a <a href="https://ro-che.info/articles/2017-06-17-generic-unification">blog post by Roman Cheplyaka</a>, and the <a href="https://hackage.haskell.org/package/unification-fd">unification-fd Haddock documentation</a> itself. I also referred to the <a href="https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system">Wikipedia page on Hindley-Milner</a>, which is extremely thorough.</p>
<p>This blog post is rendered automatically from a literate Haskell file; you can <a href="https://github.com/byorgey/hm-unification-fd">find the complete working source code and blog post on GitHub</a>. I’m always happy to receive comments, fixes, or suggestions for improvement.</p>
<h2 id="prelude-a-bunch-of-extensions-and-imports">Prelude: A Bunch of Extensions and Imports</h2>
<p>We will make GHC and other people’s libraries work very hard for us. You know the drill.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveAnyClass        #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveFoldable        #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveFunctor         #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveGeneric         #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveTraversable     #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE FlexibleContexts      #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE FlexibleInstances     #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE GADTs                 #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE LambdaCase            #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE MultiParamTypeClasses #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE PatternSynonyms       #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE StandaloneDeriving    #-}</span>
<span>&gt;</span> <span style="color:green;">{-# LANGUAGE UndecidableInstances  #-}</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control.Category</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>&gt;&gt;&gt;</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control.Monad.Except</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control.Monad.Reader</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data.Foldable</span>              <span style="color:red;">(</span><span>fold</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data.Functor.Identity</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data.List</span>                  <span style="color:red;">(</span><span>intercalate</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data.Map</span>                   <span style="color:red;">(</span><span>Map</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.Map</span>                   <span style="color:blue;font-weight:bold;">as</span> <span>M</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data.Maybe</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data.Set</span>                   <span style="color:red;">(</span><span>Set</span><span style="color:red;">,</span> <span style="color:red;">(</span><span>\\</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.Set</span>                   <span style="color:blue;font-weight:bold;">as</span> <span>S</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Prelude</span>                    <span>hiding</span> <span style="color:red;">(</span><span>lookup</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Text.Printf</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec.Expr</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec.Language</span>       <span style="color:red;">(</span><span>emptyDef</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Text.Parsec.String</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Text.Parsec.Token</span>          <span style="color:blue;font-weight:bold;">as</span> <span>L</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control.Unification</span>        <span>hiding</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>=:=</span><span style="color:red;">)</span><span style="color:red;">,</span> <span>applyBindings</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Control.Unification</span>        <span style="color:blue;font-weight:bold;">as</span> <span>U</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control.Unification.IntVar</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data.Functor.Fixedpoint</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>GHC.Generics</span>               <span style="color:red;">(</span><span>Generic1</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>System.Console.Repline</span>
</code></pre>
<h2 id="representing-our-types">Representing our types</h2>
<p>We’ll be implementing a language with lambas, application, and let-expressions—as well as natural numbers with an addition operation, just to give us a base type and something to do with it. So we will have a natural number type and function types, along with polymorphism, <em>i.e.</em> type variables and <code>forall</code>. (Adding more features like sum and product types, additional base types, recursion, <em>etc.</em> is left as an exercise for the reader!)</p>
<p>So notionally, we want something like this:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">type</span> <span>Var</span> <span style="color:red;">=</span> <span>String</span>
<span style="color:blue;font-weight:bold;">data</span> <span>Type</span> <span style="color:red;">=</span> <span>TyVar</span> <span>Var</span> <span style="color:red;">|</span> <span>TyNat</span> <span style="color:red;">|</span> <span>TyFun</span> <span>Type</span> <span>Type</span></code></pre>
<p>However, when using <code>unification-fd</code>, we have to encode our <code>Type</code> data type (<em>i.e.</em> the thing we want to do unification on) using a “two-level type” (see <a href="http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.83.500">Tim Sheard’s original paper</a>).</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>Var</span> <span style="color:red;">=</span> <span>String</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>TypeF</span> <span>a</span> <span style="color:red;">=</span> <span>TyVarF</span> <span>Var</span> <span style="color:red;">|</span> <span>TyNatF</span> <span style="color:red;">|</span> <span>TyFunF</span> <span>a</span> <span>a</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Eq</span><span style="color:red;">,</span> <span>Functor</span><span style="color:red;">,</span> <span>Foldable</span><span style="color:red;">,</span> <span>Traversable</span><span style="color:red;">,</span> <span>Generic1</span><span style="color:red;">,</span> <span>Unifiable</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>Type</span> <span style="color:red;">=</span> <span>Fix</span> <span>TypeF</span>
</code></pre>
<p><code>TypeF</code> is a “structure functor” that just defines a single level of structure; notice <code>TypeF</code> is not recursive at all, but uses the type parameter <code>a</code> to mark the places where a recursive instance would usually be. <code>unification-fd</code> provides a <code>Fix</code> type to “tie the knot” and make it recursive. (I’m not sure why <code>unification-fd</code> defines its own <code>Fix</code> type instead of using the one from <code>Data.Fix</code>, but perhaps the reason is that it was written before <code>Data.Fix</code> existed—<code>unification-fd</code> was first published in 2007!)</p>
<p>We have to derive a whole bunch of instances for <code>TypeF</code> which fortunately we get for free. Note in particular <code>Generic1</code> and <code>Unifiable</code>: <code>Unifiable</code> is a type class from <code>unification-fd</code> which describes how to match up values of our type. Thanks to the <a href="https://ro-che.info/articles/2017-06-17-generic-unification">work of Roman Cheplyaka</a>, there is a default implementation for <code>Unifiable</code> based on a <code>Generic1</code> instance—which GHC derives for us in turn—and the default implementation works great for our purposes.</p>
<p><code>unification-fd</code> also provides a second type for tying the knot, called <code>UTerm</code>, defined like so:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">data</span> <span>UTerm</span> <span>t</span> <span>v</span>
  <span style="color:red;">=</span> <span>UVar</span>  <span>!</span><span>v</span>               <span style="color:green;">-- ^ A unification variable.</span>
  <span style="color:red;">|</span> <span>UTerm</span> <span>!</span><span style="color:red;">(</span><span>t</span> <span style="color:red;">(</span><span>UTerm</span> <span>t</span> <span>v</span><span style="color:red;">)</span><span style="color:red;">)</span> <span style="color:green;">-- ^ Some structure containing subterms.</span></code></pre>
<p>It’s similar to <code>Fix</code>, except it also adds unification variables of some type <code>v</code>. (If it means anything to you, note that <code>UTerm</code> is actually the <em>free monad</em> over <code>t</code>.) We also define a version of <code>Type</code> using <code>UTerm</code>, which we will use during type inference:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>UType</span> <span style="color:red;">=</span> <span>UTerm</span> <span>TypeF</span> <span>IntVar</span>
</code></pre>
<p><code>IntVar</code> is a type provided by <code>unification-fd</code> representing variables as <code>Int</code> values, with a mapping from variables to bindings stored in an <code>IntMap</code>. <code>unification-fd</code> also provies an <code>STVar</code> type which implements variables via <code>STRef</code>s; I presume using <code>STVar</code>s would be faster (since no intermediate lookups in an <code>IntMap</code> are required) but forces us to work in the <code>ST</code> monad. For now I will just stick with <code>IntVar</code>, which makes things simpler.</p>
<p>At this point you might wonder: why do we have type variables in our definition of <code>TypeF</code>, but <em>also</em> use <code>UTerm</code> to add unification variables? Can’t we just get rid of the <code>TyVarF</code> constructor, and let <code>UTerm</code> provide the variables? Well, type variables and unification variables are subtly different, though intimately related. A type variable is actually part of a type, whereas a unification variable is not itself a type, but only <em>stands for</em> some type which is (as yet) unknown. After we are completely done with type inference, we won’t have a <code>UTerm</code> any more, but we might have a type like <code>forall a. a -&gt; a</code> which still contains type variables, so we need a way to represent them. We could only get rid of the <code>TyVarF</code> constructor if we were doing type inference for a language without polymorphism (and yes, unification could still be helpful in such a situation—for example, to do full type reconstruction for the simply typed lambda calculus, where lambdas do not have type annotations).</p>
<p><code>Polytype</code> represents a polymorphic type, with a <code>forall</code> at the front and a list of bound type variables (note that regular monomorphic types can be represented as <code>Forall [] ty</code>). We don’t need to make an instance of <code>Unifiable</code> for <code>Polytype</code>, since we never unify polytypes, only (mono)types. However, we can have polytypes with unification variables in them, so we need two versions, one containing a <code>Type</code> and one containing a <code>UType</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Poly</span> <span>t</span> <span style="color:red;">=</span> <span>Forall</span> <span style="color:red;">[</span><span>Var</span><span style="color:red;">]</span> <span>t</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Eq</span><span style="color:red;">,</span> <span>Show</span><span style="color:red;">,</span> <span>Functor</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>Polytype</span>  <span style="color:red;">=</span> <span>Poly</span> <span>Type</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>UPolytype</span> <span style="color:red;">=</span> <span>Poly</span> <span>UType</span>
</code></pre>
<p>Finally, for convenience, we can make a bunch of pattern synonyms that let us work with <code>Type</code> and <code>UType</code> just as if they were directly recursive types. This isn’t required; I just like not having to write <code>Fix</code> and <code>UTerm</code> everywhere.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>pattern</span> <span>TyVar</span> <span style="color:red;">::</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>Type</span>
<span>&gt;</span> <span>pattern</span> <span>TyVar</span> <span>v</span> <span style="color:red;">=</span> <span>Fix</span> <span style="color:red;">(</span><span>TyVarF</span> <span>v</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>pattern</span> <span>TyNat</span> <span style="color:red;">::</span> <span>Type</span>
<span>&gt;</span> <span>pattern</span> <span>TyNat</span> <span style="color:red;">=</span> <span>Fix</span> <span>TyNatF</span>
<span>&gt;</span> 
<span>&gt;</span> <span>pattern</span> <span>TyFun</span> <span style="color:red;">::</span> <span>Type</span> <span style="color:red;">-&gt;</span> <span>Type</span> <span style="color:red;">-&gt;</span> <span>Type</span>
<span>&gt;</span> <span>pattern</span> <span>TyFun</span> <span>t1</span> <span>t2</span> <span style="color:red;">=</span> <span>Fix</span> <span style="color:red;">(</span><span>TyFunF</span> <span>t1</span> <span>t2</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>pattern</span> <span>UTyNat</span> <span style="color:red;">::</span> <span>UType</span>
<span>&gt;</span> <span>pattern</span> <span>UTyNat</span> <span style="color:red;">=</span> <span>UTerm</span> <span>TyNatF</span>
<span>&gt;</span> 
<span>&gt;</span> <span>pattern</span> <span>UTyFun</span> <span style="color:red;">::</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>UType</span>
<span>&gt;</span> <span>pattern</span> <span>UTyFun</span> <span>t1</span> <span>t2</span> <span style="color:red;">=</span> <span>UTerm</span> <span style="color:red;">(</span><span>TyFunF</span> <span>t1</span> <span>t2</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>pattern</span> <span>UTyVar</span> <span style="color:red;">::</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>UType</span>
<span>&gt;</span> <span>pattern</span> <span>UTyVar</span> <span>v</span> <span style="color:red;">=</span> <span>UTerm</span> <span style="color:red;">(</span><span>TyVarF</span> <span>v</span><span style="color:red;">)</span>
</code></pre>
<h2 id="expressions">Expressions</h2>
<p>Here’s a data type to represent expressions. There’s nothing much interesting to see here, since we don’t need to do anything fancy with expressions. Note that lambdas don’t have type annotations, but <code>let</code>-expressions can have an optional polytype annotation, which will let us talk about <em>checking</em> polymorphic types in addition to inferring them (a lot of presentations of Hindley-Milner don’t talk about this).</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Expr</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>EVar</span>  <span style="color:red;">::</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>Expr</span>
<span>&gt;</span>   <span>EInt</span>  <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Expr</span>
<span>&gt;</span>   <span>EPlus</span> <span style="color:red;">::</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Expr</span>
<span>&gt;</span>   <span>ELam</span>  <span style="color:red;">::</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Expr</span>
<span>&gt;</span>   <span>EApp</span>  <span style="color:red;">::</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Expr</span>
<span>&gt;</span>   <span>ELet</span>  <span style="color:red;">::</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>Maybe</span> <span>Polytype</span> <span style="color:red;">-&gt;</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Expr</span>
</code></pre>
<p>Normally at this point we would write parsers and pretty-printers for types and expressions, but that’s boring and has very little to do with <code>unification-fd</code>, so I’ve left those to the end. Let’s get on with the interesting bits!</p>
<h2 id="type-inference-infrastructure">Type inference infrastructure</h2>
<p>Before we get to the type inference algorithm proper, we’ll need to develop a bunch of infrastructure. First, here’s the concrete monad we will be using for type inference. The <code>ReaderT Ctx</code> will keep track of variables and their types; <code>ExceptT TypeError</code> of course allows us to fail with type errors; and <code>IntBindingT</code> is a monad transformer provided by <code>unification-fd</code> which supports various operations such as generating fresh variables and unifying things. Note, for reasons that will become clear later, it’s very important that the <code>IntBindingT</code> is on the bottom of the stack, and the <code>ExceptT</code> comes right above it. Beyond that we can add whatever we like on top.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>Infer</span> <span style="color:red;">=</span> <span>ReaderT</span> <span>Ctx</span> <span style="color:red;">(</span><span>ExceptT</span> <span>TypeError</span> <span style="color:red;">(</span><span>IntBindingT</span> <span>TypeF</span> <span>Identity</span><span style="color:red;">)</span><span style="color:red;">)</span>
</code></pre>
<p>Normally, I would prefer to write everything in a “capability style” where the code is polymorphic in the monad, and just specifies what capabilites/effects it needs (either just using <code>mtl</code> classes directly, or using an effects library like <code>polysemy</code> or <code>fused-effects</code>), but the way the <code>unification-fd</code> API is designed seems to make that a bit tricky.</p>
<p>A type context is a mapping from variable names to polytypes; we also have a function for looking up the type of a variable in the context, and a function for running a local subcomputation in an extended context.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>Ctx</span> <span style="color:red;">=</span> <span>Map</span> <span>Var</span> <span>UPolytype</span>
<span>&gt;</span> 
<span>&gt;</span> <span>lookup</span> <span style="color:red;">::</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>UType</span>
<span>&gt;</span> <span>lookup</span> <span>x</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>ctx</span> <span style="color:red;">&lt;-</span> <span>ask</span>
<span>&gt;</span>   <span>maybe</span> <span style="color:red;">(</span><span>throwError</span> <span>$</span> <span>UnboundVar</span> <span>x</span><span style="color:red;">)</span> <span>instantiate</span> <span style="color:red;">(</span><span>M.lookup</span> <span>x</span> <span>ctx</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>withBinding</span> <span style="color:red;">::</span> <span>MonadReader</span> <span>Ctx</span> <span>m</span> <span style="color:red;">=&gt;</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>UPolytype</span> <span style="color:red;">-&gt;</span> <span>m</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>m</span> <span>a</span>
<span>&gt;</span> <span>withBinding</span> <span>x</span> <span>ty</span> <span style="color:red;">=</span> <span>local</span> <span style="color:red;">(</span><span>M.insert</span> <span>x</span> <span>ty</span><span style="color:red;">)</span>
</code></pre>
<p>The <code>lookup</code> function throws an error if the variable is not in the context, and otherwise returns a <code>UType</code>. Conversion from the <code>UPolytype</code> stored in the context to a <code>UType</code> happens via a function called <code>instantiate</code>, which opens up the <code>UPolytype</code> and replaces each of the variables bound by the <code>forall</code> with a fresh unification variable. We will see the implementation of <code>instantiate</code> later.</p>
<p>We will often need to recurse over <code>UType</code>s. We could just write directly recursive functions ourselves, but there is a better way. Although the <code>unification-fd</code> library provides a function <code>cata</code> for doing a fold over a term built with <code>Fix</code>, it doesn’t provide a counterpart for <code>UTerm</code>; but no matter, we can write one ourselves, like so:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>ucata</span> <span style="color:red;">::</span> <span>Functor</span> <span>t</span> <span style="color:red;">=&gt;</span> <span style="color:red;">(</span><span>v</span> <span style="color:red;">-&gt;</span> <span>a</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>t</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>a</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>UTerm</span> <span>t</span> <span>v</span> <span style="color:red;">-&gt;</span> <span>a</span>
<span>&gt;</span> <span>ucata</span> <span>f</span> <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">(</span><span>UVar</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>f</span> <span>v</span>
<span>&gt;</span> <span>ucata</span> <span>f</span> <span>g</span> <span style="color:red;">(</span><span>UTerm</span> <span>t</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>g</span> <span style="color:red;">(</span><span>fmap</span> <span style="color:red;">(</span><span>ucata</span> <span>f</span> <span>g</span><span style="color:red;">)</span> <span>t</span><span style="color:red;">)</span>
</code></pre>
<p>Now, we can write some utilities for finding free variables. Inexplicably, <code>IntVar</code> does not have an <code>Ord</code> instance (even though it is literally just a <code>newtype</code> over <code>Int</code>), so we have to derive one if we want to store them in a <code>Set</code>. Notice that our <code>freeVars</code> function finds free unification variables <em>and</em> free type variables; I will talk about why we need this later (this is something I got wrong at first!).</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Ord</span> <span>IntVar</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">class</span> <span>FreeVars</span> <span>a</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>freeVars</span> <span style="color:red;">::</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span style="color:red;">(</span><span>Set</span> <span style="color:red;">(</span><span>Either</span> <span>Var</span> <span>IntVar</span><span style="color:red;">)</span><span style="color:red;">)</span>
</code></pre>
<p>Finding the free variables in a <code>UType</code> is our first application of <code>ucata</code>. First, to find the free unification variables, we just use the <code>getFreeVars</code> function provided by <code>unification-fd</code> and massage the output into the right form. To find free type variables, we fold using <code>ucata</code>: we ignore unification variables, capture a singleton set in the <code>TyVarF</code> case, and in the recursive case we call <code>fold</code>, which will turn a <code>TypeF (Set ...)</code> into a <code>Set ...</code> using the <code>Monoid</code> instance for <code>Set</code>, <em>i.e.</em> <code>union</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>FreeVars</span> <span>UType</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>freeVars</span> <span>ut</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>     <span>fuvs</span> <span style="color:red;">&lt;-</span> <span>fmap</span> <span style="color:red;">(</span><span>S.fromList</span> <span>.</span> <span>map</span> <span>Right</span><span style="color:red;">)</span> <span>.</span> <span>lift</span> <span>.</span> <span>lift</span> <span>$</span> <span>getFreeVars</span> <span>ut</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">let</span> <span>ftvs</span> <span style="color:red;">=</span> <span>ucata</span> <span style="color:red;">(</span><span>const</span> <span>S.empty</span><span style="color:red;">)</span>
<span>&gt;</span>                      <span style="color:red;">(</span><span style="color:red;">\</span><span style="color:blue;font-weight:bold;">case</span> <span style="color:red;">{</span><span>TyVarF</span> <span>x</span> <span style="color:red;">-&gt;</span> <span>S.singleton</span> <span style="color:red;">(</span><span>Left</span> <span>x</span><span style="color:red;">)</span><span style="color:red;">;</span> <span>f</span> <span style="color:red;">-&gt;</span> <span>fold</span> <span>f</span><span style="color:red;">}</span><span style="color:red;">)</span>
<span>&gt;</span>                      <span>ut</span>
<span>&gt;</span>     <span>return</span> <span>$</span> <span>fuvs</span> <span>`S.union`</span> <span>ftvs</span>
</code></pre>
<p>Why don’t we just find free unification variables with <code>ucata</code> at the same time as the free type variables, and forget about using <code>getFreeVars</code>? Well, I looked at the source, and <code>getFreeVars</code> is actually a complicated beast. I’m really not sure what it’s doing, and I don’t trust that just manually getting the unification variables ourselves would be doing the right thing!</p>
<p>Now we can leverage the above instance to find free varaibles in <code>UPolytype</code>s and type contexts. For a <code>UPolytype</code>, we of course have to subtract off any variables bound by the <code>forall</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>FreeVars</span> <span>UPolytype</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>freeVars</span> <span style="color:red;">(</span><span>Forall</span> <span>xs</span> <span>ut</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>\\</span> <span style="color:red;">(</span><span>S.fromList</span> <span style="color:red;">(</span><span>map</span> <span>Left</span> <span>xs</span><span style="color:red;">)</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>&lt;$&gt;</span> <span>freeVars</span> <span>ut</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>FreeVars</span> <span>Ctx</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>freeVars</span> <span style="color:red;">=</span> <span>fmap</span> <span>S.unions</span> <span>.</span> <span>mapM</span> <span>freeVars</span> <span>.</span> <span>M.elems</span>
</code></pre>
<p>And here’s a simple utility function to generate fresh unification variables, built on top of the <code>freeVar</code> function provided by <code>unification-fd</code>:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>fresh</span> <span style="color:red;">::</span> <span>Infer</span> <span>UType</span>
<span>&gt;</span> <span>fresh</span> <span style="color:red;">=</span> <span>UVar</span> <span>&lt;$&gt;</span> <span>lift</span> <span style="color:red;">(</span><span>lift</span> <span>freeVar</span><span style="color:red;">)</span>
</code></pre>
<p>One thing to note is the annoying calls to <code>lift</code> we have to do in the definition of <code>FreeVars</code> for <code>UType</code>, and in the definition of <code>fresh</code>. The <code>getFreeVars</code> and <code>freeVar</code> functions provided by <code>unification-fv</code> have to run in a monad which is an instance of <code>BindingMonad</code>, and the <code>BindingMonad</code> class does not provide instances for <code>mtl</code> transformers. We could write our own instances so that these functions would work in our <code>Infer</code> monad automatically, but honestly that sounds like a lot of work. Sprinkling a few <code>lift</code>s here and there isn’t so bad.</p>
<p>Next, a data type to represent type errors, and an instance of the <code>Fallible</code> class, needed so that <code>unification-fd</code> can inject errors into our error type when it encounters unification errors. Basically we just need to provide two specific constructors to represent an “occurs check” failure (<em>i.e.</em> an infinite type), or a unification mismatch failure.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>TypeError</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>UnboundVar</span>   <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>TypeError</span>
<span>&gt;</span>   <span>Infinite</span>     <span style="color:red;">::</span> <span>IntVar</span> <span style="color:red;">-&gt;</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>TypeError</span>
<span>&gt;</span>   <span>Mismatch</span>     <span style="color:red;">::</span> <span>TypeF</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>TypeF</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>TypeError</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Fallible</span> <span>TypeF</span> <span>IntVar</span> <span>TypeError</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>occursFailure</span>   <span style="color:red;">=</span> <span>Infinite</span>
<span>&gt;</span>   <span>mismatchFailure</span> <span style="color:red;">=</span> <span>Mismatch</span>
</code></pre>
<p>The <code>=:=</code> operator provided by <code>unification-fd</code> is how we unify two types. It has a kind of bizarre type:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:red;">(</span><span>=:=</span><span style="color:red;">)</span> <span style="color:red;">::</span> <span style="color:red;">(</span> <span>BindingMonad</span> <span>t</span> <span>v</span> <span>m</span><span style="color:red;">,</span> <span>Fallible</span> <span>t</span> <span>v</span> <span>e</span>
         <span style="color:red;">,</span> <span>MonadTrans</span> <span>em</span><span style="color:red;">,</span> <span>Functor</span> <span style="color:red;">(</span><span>em</span> <span>m</span><span style="color:red;">)</span><span style="color:red;">,</span> <span>MonadError</span> <span>e</span> <span style="color:red;">(</span><span>em</span> <span>m</span><span style="color:red;">)</span><span style="color:red;">)</span>
      <span style="color:red;">=&gt;</span> <span>UTerm</span> <span>t</span> <span>v</span> <span style="color:red;">-&gt;</span> <span>UTerm</span> <span>t</span> <span>v</span> <span style="color:red;">-&gt;</span> <span>em</span> <span>m</span> <span style="color:red;">(</span><span>UTerm</span> <span>t</span> <span>v</span><span style="color:red;">)</span></code></pre>
<p>(Apparently I am not the only one who thinks this type is bizarre; the <code>unification-fd</code> source code contains the comment <code>-- TODO: what was the reason for the MonadTrans madness?</code>)</p>
<p>I had to stare at this for a while to understand it. It says that the output will be in some <code>BindingMonad</code> (such as <code>IntBindingT</code>), and there must be a single error monad transformer on top of it, with an error type that implements <code>Fallible</code>. So <code>=:=</code> can return <code>ExceptT TypeError (IntBindingT Identity) UType</code>, but it cannot be used directly in our <code>Infer</code> monad, because that has a <code>ReaderT</code> on top of the <code>ExceptT</code>. So I just made my own version with an extra <code>lift</code> to get it to work directly in the <code>Infer</code> monad. While we’re at it, we’ll make a lifted version of <code>applyBindings</code>, which has the same issue.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:red;">(</span><span>=:=</span><span style="color:red;">)</span> <span style="color:red;">::</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>UType</span>
<span>&gt;</span> <span>s</span> <span>=:=</span> <span>t</span> <span style="color:red;">=</span> <span>lift</span> <span>$</span> <span>s</span> <span>U</span><span>.=:=</span> <span>t</span>
<span>&gt;</span> 
<span>&gt;</span> <span>applyBindings</span> <span style="color:red;">::</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>UType</span>
<span>&gt;</span> <span>applyBindings</span> <span style="color:red;">=</span> <span>lift</span> <span>.</span> <span>U.applyBindings</span>
</code></pre>
<h2 id="converting-between-mono--and-polytypes">Converting between mono- and polytypes</h2>
<p>Central to the way Hindley-Milner works is the way we move back and forth between polytypes and monotypes. First, let’s see how to turn <code>UPolytype</code>s into <code>UType</code>s, hinted at earlier in the definition of the <code>lookup</code> function. To <code>instantiate</code> a <code>UPolytype</code>, we generate a fresh unification variable for each variable bound by the <code>Forall</code>, and then substitute them throughout the type.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>instantiate</span> <span style="color:red;">::</span> <span>UPolytype</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>UType</span>
<span>&gt;</span> <span>instantiate</span> <span style="color:red;">(</span><span>Forall</span> <span>xs</span> <span>uty</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>xs'</span> <span style="color:red;">&lt;-</span> <span>mapM</span> <span style="color:red;">(</span><span>const</span> <span>fresh</span><span style="color:red;">)</span> <span>xs</span>
<span>&gt;</span>   <span>return</span> <span>$</span> <span>substU</span> <span style="color:red;">(</span><span>M.fromList</span> <span style="color:red;">(</span><span>zip</span> <span style="color:red;">(</span><span>map</span> <span>Left</span> <span>xs</span><span style="color:red;">)</span> <span>xs'</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>uty</span>
</code></pre>
<p>The <code>substU</code> function can substitute for either kind of variable in a <code>UType</code> (right now we only need it to substitute for type variables, but we will need it to substitute for unification variables later). Of course, it is implemented via <code>ucata</code>. In the variable cases we make sure to leave the variable alone if it is not a key in the given substitution mapping. In the recursive non-variable case, we just roll up the <code>TypeF UType</code> into a <code>UType</code> by applying <code>UTerm</code>. This is the power of <code>ucata</code>: we can deal with all the boring recursive cases in one fell swoop. This function won’t have to change if we add new types to the language in the future.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>substU</span> <span style="color:red;">::</span> <span>Map</span> <span style="color:red;">(</span><span>Either</span> <span>Var</span> <span>IntVar</span><span style="color:red;">)</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>UType</span>
<span>&gt;</span> <span>substU</span> <span>m</span> <span style="color:red;">=</span> <span>ucata</span>
<span>&gt;</span>   <span style="color:red;">(</span><span style="color:red;">\</span><span>v</span> <span style="color:red;">-&gt;</span> <span>fromMaybe</span> <span style="color:red;">(</span><span>UVar</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>M.lookup</span> <span style="color:red;">(</span><span>Right</span> <span>v</span><span style="color:red;">)</span> <span>m</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span>   <span style="color:red;">(</span><span style="color:red;">\</span><span style="color:blue;font-weight:bold;">case</span>
<span>&gt;</span>       <span>TyVarF</span> <span>v</span> <span style="color:red;">-&gt;</span> <span>fromMaybe</span> <span style="color:red;">(</span><span>UTyVar</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>M.lookup</span> <span style="color:red;">(</span><span>Left</span> <span>v</span><span style="color:red;">)</span> <span>m</span><span style="color:red;">)</span>
<span>&gt;</span>       <span>f</span> <span style="color:red;">-&gt;</span> <span>UTerm</span> <span>f</span>
<span>&gt;</span>   <span style="color:red;">)</span>
</code></pre>
<p>There is one other way to convert a <code>UPolytype</code> to a <code>UType</code>, which happens when we want to <em>check</em> that an expression has a polymorphic type specified by the user. For example, <code>let foo : forall a. a -&gt; a = \x.3 in ...</code> should be a type error, because the user specified that <code>foo</code> should have type <code>forall a. a -&gt; a</code>, but then gave the implementation <code>\x.3</code> which is too specific. In this situation we can’t just <code>instantiate</code> the polytype—that would create a unification variable for <code>a</code>, and while typechecking <code>\x.3</code> it would unify <code>a</code> with <code>nat</code>. But in this case we <em>don’t</em> want <code>a</code> to unify with <code>nat</code>—it has to be held entirely abstract, because the user’s claim is that this function will work for <em>any</em> type <code>a</code>.</p>
<p>Instead of generating unification variables, we instead want to generate what are known as <em>Skolem</em> variables. Skolem variables do not unify with anything other than themselves. So how can we get <code>unification-fd</code> to do that? It does not have any built-in notion of Skolem variables. What we can do instead is to just embed the variables within the <code>UType</code> as <code>UTyVar</code>s instead of <code>UVar</code>s! <code>unification-fd</code> does not even know those are variables; it just sees them as another rigid part of the structure that must be matched exactly, just as a <code>TyFun</code> has to match another <code>TyFun</code>. The one remaining issue is that we need to generate <em>fresh</em> Skolem variables; it certainly would not do to have them collide with Skolem variables from some other <code>forall</code>. We could carry around our own supply of unique names in the <code>Infer</code> monad for this purpose, which would probably be the “proper” way to do things; but for now I did something more expedient: just get <code>unification-fd</code> to generate fresh unification variables, then rip the (unique! fresh!) <code>Int</code>s out of them and use those to make our Skolem variables.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>skolemize</span> <span style="color:red;">::</span> <span>UPolytype</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>UType</span>
<span>&gt;</span> <span>skolemize</span> <span style="color:red;">(</span><span>Forall</span> <span>xs</span> <span>uty</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>xs'</span> <span style="color:red;">&lt;-</span> <span>mapM</span> <span style="color:red;">(</span><span>const</span> <span>fresh</span><span style="color:red;">)</span> <span>xs</span>
<span>&gt;</span>   <span>return</span> <span>$</span> <span>substU</span> <span style="color:red;">(</span><span>M.fromList</span> <span style="color:red;">(</span><span>zip</span> <span style="color:red;">(</span><span>map</span> <span>Left</span> <span>xs</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>map</span> <span>toSkolem</span> <span>xs'</span><span style="color:red;">)</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>uty</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>     <span>toSkolem</span> <span style="color:red;">(</span><span>UVar</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>UTyVar</span> <span style="color:red;">(</span><span>mkVarName</span> <span style="color:teal;">"s"</span> <span>v</span><span style="color:red;">)</span>
</code></pre>
<p>When <code>unification-fd</code> generates fresh <code>IntVars</code> it seems that it starts at <code>minBound :: Int</code> and increments, so we can add <code>maxBound + 1</code> to get numbers that look reasonable. Again, this is not very principled, but for this toy example, who cares?</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>mkVarName</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>IntVar</span> <span style="color:red;">-&gt;</span> <span>Var</span>
<span>&gt;</span> <span>mkVarName</span> <span>nm</span> <span style="color:red;">(</span><span>IntVar</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>nm</span> <span>++</span> <span>show</span> <span style="color:red;">(</span><span>v</span> <span>+</span> <span style="color:red;">(</span><span>maxBound</span> <span style="color:red;">::</span> <span>Int</span><span style="color:red;">)</span> <span>+</span> <span class="hs-num">1</span><span style="color:red;">)</span>
</code></pre>
<p>Next, how do we convert from a <code>UType</code> back to a <code>UPolytype</code>? This happens when we have inferred the type of a <code>let</code>-bound variable and go to put it in the context; typically, Hindley-Milner systems <em>generalize</em> the inferred type to a polytype. If a unification variable still occurs free in a type, it means it was not constrained at all, so we can universally quantify over it. However, we have to be careful: unification variables that occur in some type that is already in the context do not count as free, because we might later discover that they need to be constrained.</p>
<p>Also, just before we do the generalization, it’s very important that we use <code>applyBindings</code>. <code>unification-fd</code> has been collecting a substitution from unification variables to types, but for efficiency’s sake it does not actually apply the substitution until we ask it to, by calling <code>applyBindings</code>. Any unification variables which still remain after <code>applyBindings</code> really are unconstrained so far. So after <code>applyBindings</code>, we get the free unification variables from the type, subtract off any unification variables which are free in the context, and close over the remaining variables with a <code>forall</code>, substituting normal type variables for them. It does not particularly matter if these type variables are fresh (so long as they are distinct). But we can’t look only at <em>unification</em> variables! We have to look at free type variables too (this is the reason that our <code>freeVars</code> function needs to find both free type and unification variables). Why is that? Well, we might have some free type variables floating around if we previously generated some Skolem variables while checking a polymorphic type. (A term which illustrates this behavior is <code>\y. let x : forall a. a -&gt; a = y in x 3</code>.) Free Skolem variables should also be generalized over.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>generalize</span> <span style="color:red;">::</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>UPolytype</span>
<span>&gt;</span> <span>generalize</span> <span>uty</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>uty'</span> <span style="color:red;">&lt;-</span> <span>applyBindings</span> <span>uty</span>
<span>&gt;</span>   <span>ctx</span> <span style="color:red;">&lt;-</span> <span>ask</span>
<span>&gt;</span>   <span>tmfvs</span>  <span style="color:red;">&lt;-</span> <span>freeVars</span> <span>uty'</span>
<span>&gt;</span>   <span>ctxfvs</span> <span style="color:red;">&lt;-</span> <span>freeVars</span> <span>ctx</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">let</span> <span>fvs</span> <span style="color:red;">=</span> <span>S.toList</span> <span>$</span> <span>tmfvs</span> <span>\\</span> <span>ctxfvs</span>
<span>&gt;</span>       <span>xs</span>  <span style="color:red;">=</span> <span>map</span> <span style="color:red;">(</span><span>either</span> <span>id</span> <span style="color:red;">(</span><span>mkVarName</span> <span style="color:teal;">"a"</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>fvs</span>
<span>&gt;</span>   <span>return</span> <span>$</span> <span>Forall</span> <span>xs</span> <span style="color:red;">(</span><span>substU</span> <span style="color:red;">(</span><span>M.fromList</span> <span style="color:red;">(</span><span>zip</span> <span>fvs</span> <span style="color:red;">(</span><span>map</span> <span>UTyVar</span> <span>xs</span><span style="color:red;">)</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>uty'</span><span style="color:red;">)</span>
</code></pre>
<p>Finally, we need a way to convert <code>Polytype</code>s entered by the user into <code>UPolytypes</code>, and a way to convert the final <code>UPolytype</code> back into a <code>Polytype</code>. <code>unification-fd</code> provides functions <code>unfreeze : Fix t -&gt; UTerm t v</code> and <code>freeze : UTerm t v -&gt; Maybe (Fix t)</code> to convert between terms built with <code>UTerm</code> (with unification variables) and <code>Fix</code> (without unification variables). Converting to <code>UPolytype</code> is easy: we just use <code>unfreeze</code> to convert the underlying <code>Type</code> to a <code>UType</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>toUPolytype</span> <span style="color:red;">::</span> <span>Polytype</span> <span style="color:red;">-&gt;</span> <span>UPolytype</span>
<span>&gt;</span> <span>toUPolytype</span> <span style="color:red;">=</span> <span>fmap</span> <span>unfreeze</span>
</code></pre>
<p>When converting back, notice that <code>freeze</code> returns a <code>Maybe</code>; it fails if there are any unification variables remaining. So we must be careful to only use <code>fromUPolytype</code> when we know there are no unification variables remaining in a polytype. In fact, we will use this only at the very top level, after generalizing the type that results from inference over a top-level term. Since at the top level we only perform inference on closed terms, in an empty type context, the final <code>generalize</code> step will generalize over all the remaining free unification variables, since there will be no free variables in the context.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>fromUPolytype</span> <span style="color:red;">::</span> <span>UPolytype</span> <span style="color:red;">-&gt;</span> <span>Polytype</span>
<span>&gt;</span> <span>fromUPolytype</span> <span style="color:red;">=</span> <span>fmap</span> <span style="color:red;">(</span><span>fromJust</span> <span>.</span> <span>freeze</span><span style="color:red;">)</span>
</code></pre>
<h2 id="type-inference">Type inference</h2>
<p>Finally, the type inference algorithm proper! First, to <code>check</code> that an expression has a given type, we <code>infer</code> the type of the expression and then demand (via <code>=:=</code>) that the inferred type must be equal to the given one. Note that <code>=:=</code> actually returns a <code>UType</code>, and it can apparently be more efficient to use the result of <code>=:=</code> in preference to either of the arguments to it (although they will all give equivalent results). However, in our case this doesn’t seem to make much difference.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>check</span> <span style="color:red;">::</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>()</span>
<span>&gt;</span> <span>check</span> <span>e</span> <span>ty</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>ty'</span> <span style="color:red;">&lt;-</span> <span>infer</span> <span>e</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">&lt;-</span> <span>ty</span> <span>=:=</span> <span>ty'</span>
<span>&gt;</span>   <span>return</span> <span>()</span>
</code></pre>
<p>And now for the <code>infer</code> function. The <code>EVar</code>, <code>EInt</code>, and <code>EPlus</code> cases are straightforward.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>infer</span> <span style="color:red;">::</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Infer</span> <span>UType</span>
<span>&gt;</span> <span>infer</span> <span style="color:red;">(</span><span>EVar</span> <span>x</span><span style="color:red;">)</span>      <span style="color:red;">=</span> <span>lookup</span> <span>x</span>
<span>&gt;</span> <span>infer</span> <span style="color:red;">(</span><span>EInt</span> <span style="color:blue;font-weight:bold;">_</span><span style="color:red;">)</span>      <span style="color:red;">=</span> <span>return</span> <span>UTyNat</span>
<span>&gt;</span> <span>infer</span> <span style="color:red;">(</span><span>EPlus</span> <span>e1</span> <span>e2</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>check</span> <span>e1</span> <span>UTyNat</span>
<span>&gt;</span>   <span>check</span> <span>e2</span> <span>UTyNat</span>
<span>&gt;</span>   <span>return</span> <span>UTyNat</span>
</code></pre>
<p>For an application <code>EApp e1 e2</code>, we infer the types <code>funTy</code> and <code>argTy</code> of <code>e1</code> and <code>e2</code> respectively, then demand that <code>funTy =:= UTyFun argTy resTy</code> for a fresh unification variable <code>resTy</code>. Again, <code>=:=</code> returns a more efficient <code>UType</code> which is equivalent to <code>funTy</code>, but we don’t need to use that type directly (we return <code>resTy</code> instead), so we just discard the result.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>infer</span> <span style="color:red;">(</span><span>EApp</span> <span>e1</span> <span>e2</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>funTy</span> <span style="color:red;">&lt;-</span> <span>infer</span> <span>e1</span>
<span>&gt;</span>   <span>argTy</span> <span style="color:red;">&lt;-</span> <span>infer</span> <span>e2</span>
<span>&gt;</span>   <span>resTy</span> <span style="color:red;">&lt;-</span> <span>fresh</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">&lt;-</span> <span>funTy</span> <span>=:=</span> <span>UTyFun</span> <span>argTy</span> <span>resTy</span>
<span>&gt;</span>   <span>return</span> <span>resTy</span>
</code></pre>
<p>For a lambda, we make up a fresh unification variable for the type of the argument, then infer the type of the body under an extended context. Notice how we promote the freshly generated unification variable to a <code>UPolytype</code> by wrapping it in <code>Forall []</code>; we do <strong>not</strong> <code>generalize</code> it, since that would turn it into <code>forall a. a</code>! We just want the lambda argument to have the bare unification variable as its type.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>infer</span> <span style="color:red;">(</span><span>ELam</span> <span>x</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>argTy</span> <span style="color:red;">&lt;-</span> <span>fresh</span>
<span>&gt;</span>   <span>withBinding</span> <span>x</span> <span style="color:red;">(</span><span>Forall</span> <span>[]</span> <span>argTy</span><span style="color:red;">)</span> <span>$</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>     <span>resTy</span> <span style="color:red;">&lt;-</span> <span>infer</span> <span>body</span>
<span>&gt;</span>     <span>return</span> <span>$</span> <span>UTyFun</span> <span>argTy</span> <span>resTy</span>
</code></pre>
<p>For a <code>let</code> expression without a type annotation, we infer the type of the definition, then generalize it and add it to the context to infer the type of the body. It is traditional for Hindley-Milner systems to generalize <code>let</code>-bound things this way (although note that GHC <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf">does not generalize <code>let</code>-bound things</a> with <code>-XMonoLocalBinds</code> enabled, which is automatically implied by <code>-XGADTs</code> or <code>-XTypeFamilies</code>).</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>infer</span> <span style="color:red;">(</span><span>ELet</span> <span>x</span> <span>Nothing</span> <span>xdef</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>ty</span> <span style="color:red;">&lt;-</span> <span>infer</span> <span>xdef</span>
<span>&gt;</span>   <span>pty</span> <span style="color:red;">&lt;-</span> <span>generalize</span> <span>ty</span>
<span>&gt;</span>   <span>withBinding</span> <span>x</span> <span>pty</span> <span>$</span> <span>infer</span> <span>body</span>
</code></pre>
<p>For a <code>let</code> expression with a type annotation, we <code>skolemize</code> it and <code>check</code> the definition with the skolemized type; the rest is the same as the previous case.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>infer</span> <span style="color:red;">(</span><span>ELet</span> <span>x</span> <span style="color:red;">(</span><span>Just</span> <span>pty</span><span style="color:red;">)</span> <span>xdef</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">let</span> <span>upty</span> <span style="color:red;">=</span> <span>toUPolytype</span> <span>pty</span>
<span>&gt;</span>   <span>upty'</span> <span style="color:red;">&lt;-</span> <span>skolemize</span> <span>upty</span>
<span>&gt;</span>   <span>check</span> <span>xdef</span> <span>upty'</span>
<span>&gt;</span>   <span>withBinding</span> <span>x</span> <span>upty</span> <span>$</span> <span>infer</span> <span>body</span>
</code></pre>
<h2 id="running-the-infer-monad">Running the <code>Infer</code> monad</h2>
<p>We need a way to run computations in the <code>Infer</code> monad. This is a bit fiddly, and it took me a long time to put all the pieces together. (But <a href="https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/typed_holes.html">typed holes</a> are sooooo great! It would have taken me way longer without them…) I’ve written the definition of <code>runInfer</code> using the backwards function composition operator, <code>(&gt;&gt;&gt;)</code>, so that the pipeline flows from top to bottom and I can intersperse it with explanation.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>runInfer</span> <span style="color:red;">::</span> <span>Infer</span> <span>UType</span> <span style="color:red;">-&gt;</span> <span>Either</span> <span>TypeError</span> <span>Polytype</span>
<span>&gt;</span> <span>runInfer</span>
</code></pre>
<p>The first thing we do is use <code>applyBindings</code> to make sure that we substitute for any unification variables that we know about. This results in another <code>Infer UType</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span>   <span style="color:red;">=</span>   <span style="color:red;">(</span><span>&gt;&gt;=</span> <span>applyBindings</span><span style="color:red;">)</span>
</code></pre>
<p>We can now generalize over any unification variables that are left, and then convert from <code>UPolytype</code> to <code>Polytype</code>. Again, this conversion is safe because at this top level we know we will be in an empty context, so the generalization step will definitely get rid of all the remaining unification variables.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span>   <span>&gt;&gt;&gt;</span> <span style="color:red;">(</span><span>&gt;&gt;=</span> <span style="color:red;">(</span><span>generalize</span> <span>&gt;&gt;&gt;</span> <span>fmap</span> <span>fromUPolytype</span><span style="color:red;">)</span><span style="color:red;">)</span>
</code></pre>
<p>Now all that’s left is to interpret the layers of our <code>Infer</code> monad one by one. As promised, we start with an empty type context.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span>   <span>&gt;&gt;&gt;</span> <span>flip</span> <span>runReaderT</span> <span>M.empty</span>
<span>&gt;</span>   <span>&gt;&gt;&gt;</span> <span>runExceptT</span>
<span>&gt;</span>   <span>&gt;&gt;&gt;</span> <span>evalIntBindingT</span>
<span>&gt;</span>   <span>&gt;&gt;&gt;</span> <span>runIdentity</span>
</code></pre>
<p>Finally, we can make a top-level function to infer a polytype for an expression, just by composing <code>infer</code> and <code>runInfer</code>.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>inferPolytype</span> <span style="color:red;">::</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Either</span> <span>TypeError</span> <span>Polytype</span>
<span>&gt;</span> <span>inferPolytype</span> <span style="color:red;">=</span> <span>runInfer</span> <span>.</span> <span>infer</span>
</code></pre>
<h2 id="repl">REPL</h2>
<p>To be able to test things out, we can make a very simple REPL that takes input from the user and tries to parse, typecheck, and interpret it, printing either the results or an appropriate error message.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>eval</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>IO</span> <span>()</span>
<span>&gt;</span> <span>eval</span> <span>s</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">case</span> <span>parse</span> <span>expr</span> <span style="color:teal;">""</span> <span>s</span> <span style="color:blue;font-weight:bold;">of</span>
<span>&gt;</span>   <span>Left</span> <span>err</span> <span style="color:red;">-&gt;</span> <span>print</span> <span>err</span>
<span>&gt;</span>   <span>Right</span> <span>e</span> <span style="color:red;">-&gt;</span> <span style="color:blue;font-weight:bold;">case</span> <span>inferPolytype</span> <span>e</span> <span style="color:blue;font-weight:bold;">of</span>
<span>&gt;</span>     <span>Left</span> <span>tyerr</span> <span style="color:red;">-&gt;</span> <span>putStrLn</span> <span>$</span> <span>pretty</span> <span>tyerr</span>
<span>&gt;</span>     <span>Right</span> <span>ty</span>   <span style="color:red;">-&gt;</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>       <span>putStrLn</span> <span>$</span> <span>pretty</span> <span>e</span> <span>++</span> <span style="color:teal;">" : "</span> <span>++</span> <span>pretty</span> <span>ty</span>
<span>&gt;</span>       <span>when</span> <span style="color:red;">(</span><span>ty</span> <span>==</span> <span>Forall</span> <span>[]</span> <span>TyNat</span><span style="color:red;">)</span> <span>$</span> <span>putStrLn</span> <span>$</span> <span>pretty</span> <span style="color:red;">(</span><span>interp</span> <span>e</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>main</span> <span style="color:red;">::</span> <span>IO</span> <span>()</span>
<span>&gt;</span> <span>main</span> <span style="color:red;">=</span> <span>evalRepl</span> <span style="color:red;">(</span><span>const</span> <span style="color:red;">(</span><span>pure</span> <span style="color:teal;">"HM&gt; "</span><span style="color:red;">)</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>liftIO</span> <span>.</span> <span>eval</span><span style="color:red;">)</span> <span>[]</span> <span>Nothing</span> <span>Nothing</span> <span style="color:red;">(</span><span>Word</span> <span style="color:red;">(</span><span>const</span> <span style="color:red;">(</span><span>return</span> <span>[]</span><span style="color:red;">)</span><span style="color:red;">)</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>return</span> <span>()</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>return</span> <span>Exit</span><span style="color:red;">)</span>
</code></pre>
<p>Here are a few examples to try out:</p>
<pre><code>HM&gt; 2 + 3
2 + 3 : nat
5
HM&gt; \x. x
\x. x : forall a0. a0 -&gt; a0
HM&gt; \x.3
\x. 3 : forall a0. a0 -&gt; nat
HM&gt; \x. x + 1
\x. x + 1 : nat -&gt; nat
HM&gt; (\x. 3) (\y.y)
(\x. 3) (\y. y) : nat
3
HM&gt; \x. y
Unbound variable y
HM&gt; \x. x x
Infinite type u0 = u0 -&gt; u1
HM&gt; 3 3
Can't unify nat and nat -&gt; u0
HM&gt; let foo : forall a. a -&gt; a = \x.3 in foo 5
Can't unify s0 and nat
HM&gt; \f.\g.\x. f (g x)
\f. \g. \x. f (g x) : forall a2 a3 a4. (a3 -&gt; a4) -&gt; (a2 -&gt; a3) -&gt; a2 -&gt; a4
HM&gt; let f : forall a. a -&gt; a = \x.x in let y : forall b. b -&gt; b -&gt; b = \z.\q. f z in y 2 3
let f : forall a. a -&gt; a = \x. x in let y : forall b. b -&gt; b -&gt; b = \z. \q. f z in y 2 3 : nat
2
HM&gt; \y. let x : forall a. a -&gt; a = y in x 3
\y. let x : forall a. a -&gt; a = y in x 3 : forall s1. (s1 -&gt; s1) -&gt; nat
HM&gt; (\x. let y = x in y) (\z. \q. z)
(\x. let y = x in y) (\z. \q. z) : forall a1 a2. a1 -&gt; a2 -&gt; a1</code></pre>
<p>And that’s it! Feel free to play around with this yourself, and adapt the code for your own projects if it’s helpful. And please do report any typos or bugs that you find.</p>
<p>Below, for completeness, you will find a simple, recursive, environment-passing interpreter, along with code for parsing and pretty-printing. I don’t give any commentary on them because, for the most part, they are straightforward and have nothing to do with <code>unification-fd</code>. But you are certainly welcome to look at them if you want to see how they work. The one interesting thing to say about the parser for types is that it checks that types entered by the user do not contain any free variables, and fails if they do. The parser is not really the right place to do this check, but again, it was expedient for this toy example. Also, I tend to use <code>megaparsec</code> for serious projects, but I had some <code>parsec</code> code for parsing something similar to this toy language lying around, so I just reused that.</p>
<h2 id="interpreter">Interpreter</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Value</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>VInt</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Value</span>
<span>&gt;</span>   <span>VClo</span> <span style="color:red;">::</span> <span>Var</span> <span style="color:red;">-&gt;</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Env</span> <span style="color:red;">-&gt;</span> <span>Value</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>Env</span> <span style="color:red;">=</span> <span>Map</span> <span>Var</span> <span>Value</span>
<span>&gt;</span> 
<span>&gt;</span> <span>interp</span> <span style="color:red;">::</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Value</span>
<span>&gt;</span> <span>interp</span> <span style="color:red;">=</span> <span>interp'</span> <span>M.empty</span>
<span>&gt;</span> 
<span>&gt;</span> <span>interp'</span> <span style="color:red;">::</span> <span>Env</span> <span style="color:red;">-&gt;</span> <span>Expr</span> <span style="color:red;">-&gt;</span> <span>Value</span>
<span>&gt;</span> <span>interp'</span> <span>env</span> <span style="color:red;">(</span><span>EVar</span> <span>x</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>fromJust</span> <span>$</span> <span>M.lookup</span> <span>x</span> <span>env</span>
<span>&gt;</span> <span>interp'</span> <span style="color:blue;font-weight:bold;">_</span>   <span style="color:red;">(</span><span>EInt</span> <span>n</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>VInt</span> <span>n</span>
<span>&gt;</span> <span>interp'</span> <span>env</span> <span style="color:red;">(</span><span>EPlus</span> <span>ea</span> <span>eb</span><span style="color:red;">)</span>   <span style="color:red;">=</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">case</span> <span style="color:red;">(</span><span>interp'</span> <span>env</span> <span>ea</span><span style="color:red;">,</span> <span>interp'</span> <span>env</span> <span>eb</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">of</span>
<span>&gt;</span>     <span style="color:red;">(</span><span>VInt</span> <span>va</span><span style="color:red;">,</span> <span>VInt</span> <span>vb</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>VInt</span> <span style="color:red;">(</span><span>va</span> <span>+</span> <span>vb</span><span style="color:red;">)</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">-&gt;</span> <span>error</span> <span style="color:teal;">"Impossible! interp' EPlus on non-Ints"</span>
<span>&gt;</span> <span>interp'</span> <span>env</span> <span style="color:red;">(</span><span>ELam</span> <span>x</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>VClo</span> <span>x</span> <span>body</span> <span>env</span>
<span>&gt;</span> <span>interp'</span> <span>env</span> <span style="color:red;">(</span><span>EApp</span> <span>fun</span> <span>arg</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">case</span> <span>interp'</span> <span>env</span> <span>fun</span> <span style="color:blue;font-weight:bold;">of</span>
<span>&gt;</span>     <span>VClo</span> <span>x</span> <span>body</span> <span>env'</span> <span style="color:red;">-&gt;</span>
<span>&gt;</span>       <span>interp'</span> <span style="color:red;">(</span><span>M.insert</span> <span>x</span> <span style="color:red;">(</span><span>interp'</span> <span>env</span> <span>arg</span><span style="color:red;">)</span> <span>env'</span><span style="color:red;">)</span> <span>body</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">-&gt;</span> <span>error</span> <span style="color:teal;">"Impossible! interp' EApp on non-closure"</span>
<span>&gt;</span> <span>interp'</span> <span>env</span> <span style="color:red;">(</span><span>ELet</span> <span>x</span> <span style="color:blue;font-weight:bold;">_</span> <span>xdef</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">let</span> <span>xval</span> <span style="color:red;">=</span> <span>interp'</span> <span>env</span> <span>xdef</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">in</span>  <span>interp'</span> <span style="color:red;">(</span><span>M.insert</span> <span>x</span> <span>xval</span> <span>env</span><span style="color:red;">)</span> <span>body</span>
</code></pre>
<h2 id="parsing">Parsing</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>lexer</span> <span style="color:red;">::</span> <span>L.TokenParser</span> <span>u</span>
<span>&gt;</span> <span>lexer</span> <span style="color:red;">=</span> <span>L.makeTokenParser</span> <span>emptyDef</span>
<span>&gt;</span>   <span style="color:red;">{</span> <span>L.reservedNames</span> <span style="color:red;">=</span> <span style="color:red;">[</span><span style="color:teal;">"let"</span><span style="color:red;">,</span> <span style="color:teal;">"in"</span><span style="color:red;">,</span> <span style="color:teal;">"forall"</span><span style="color:red;">,</span> <span style="color:teal;">"nat"</span><span style="color:red;">]</span> <span style="color:red;">}</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parens</span> <span style="color:red;">::</span> <span>Parser</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Parser</span> <span>a</span>
<span>&gt;</span> <span>parens</span> <span style="color:red;">=</span> <span>L.parens</span> <span>lexer</span>
<span>&gt;</span> 
<span>&gt;</span> <span>identifier</span> <span style="color:red;">::</span> <span>Parser</span> <span>String</span>
<span>&gt;</span> <span>identifier</span> <span style="color:red;">=</span> <span>L.identifier</span> <span>lexer</span>
<span>&gt;</span> 
<span>&gt;</span> <span>reserved</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>Parser</span> <span>()</span>
<span>&gt;</span> <span>reserved</span> <span style="color:red;">=</span> <span>L.reserved</span> <span>lexer</span>
<span>&gt;</span> 
<span>&gt;</span> <span>reservedOp</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>Parser</span> <span>()</span>
<span>&gt;</span> <span>reservedOp</span> <span style="color:red;">=</span> <span>L.reservedOp</span> <span>lexer</span>
<span>&gt;</span> 
<span>&gt;</span> <span>symbol</span> <span style="color:red;">::</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>Parser</span> <span>String</span>
<span>&gt;</span> <span>symbol</span> <span style="color:red;">=</span> <span>L.symbol</span> <span>lexer</span>
<span>&gt;</span> 
<span>&gt;</span> <span>integer</span> <span style="color:red;">::</span> <span>Parser</span> <span>Integer</span>
<span>&gt;</span> <span>integer</span> <span style="color:red;">=</span> <span>L.natural</span> <span>lexer</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parseAtom</span> <span style="color:red;">::</span> <span>Parser</span> <span>Expr</span>
<span>&gt;</span> <span>parseAtom</span>
<span>&gt;</span>   <span style="color:red;">=</span>   <span>EVar</span>  <span>&lt;$&gt;</span> <span>identifier</span>
<span>&gt;</span>   <span>&lt;|&gt;</span> <span>EInt</span>  <span>&lt;$&gt;</span> <span>integer</span>
<span>&gt;</span>   <span>&lt;|&gt;</span> <span>ELam</span>  <span>&lt;$&gt;</span> <span style="color:red;">(</span><span>symbol</span> <span style="color:teal;">"\\"</span> <span>*&gt;</span> <span>identifier</span><span style="color:red;">)</span>
<span>&gt;</span>             <span>&lt;*&gt;</span> <span style="color:red;">(</span><span>symbol</span> <span style="color:teal;">"."</span> <span>*&gt;</span> <span>parseExpr</span><span style="color:red;">)</span>
<span>&gt;</span>   <span>&lt;|&gt;</span> <span>ELet</span>  <span>&lt;$&gt;</span> <span style="color:red;">(</span><span>reserved</span> <span style="color:teal;">"let"</span> <span>*&gt;</span> <span>identifier</span><span style="color:red;">)</span>
<span>&gt;</span>             <span>&lt;*&gt;</span> <span>optionMaybe</span> <span style="color:red;">(</span><span>symbol</span> <span style="color:teal;">":"</span> <span>*&gt;</span> <span>parsePolytype</span><span style="color:red;">)</span>
<span>&gt;</span>             <span>&lt;*&gt;</span> <span style="color:red;">(</span><span>symbol</span> <span style="color:teal;">"="</span> <span>*&gt;</span> <span>parseExpr</span><span style="color:red;">)</span>
<span>&gt;</span>             <span>&lt;*&gt;</span> <span style="color:red;">(</span><span>reserved</span> <span style="color:teal;">"in"</span> <span>*&gt;</span> <span>parseExpr</span><span style="color:red;">)</span>
<span>&gt;</span>   <span>&lt;|&gt;</span> <span>parens</span> <span>parseExpr</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parseApp</span> <span style="color:red;">::</span> <span>Parser</span> <span>Expr</span>
<span>&gt;</span> <span>parseApp</span> <span style="color:red;">=</span> <span>chainl1</span> <span>parseAtom</span> <span style="color:red;">(</span><span>return</span> <span>EApp</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parseExpr</span> <span style="color:red;">::</span> <span>Parser</span> <span>Expr</span>
<span>&gt;</span> <span>parseExpr</span> <span style="color:red;">=</span> <span>buildExpressionParser</span> <span>table</span> <span>parseApp</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>     <span>table</span> <span style="color:red;">=</span> <span style="color:red;">[</span> <span style="color:red;">[</span> <span>Infix</span> <span style="color:red;">(</span><span>EPlus</span> <span>&lt;$</span> <span>reservedOp</span> <span style="color:teal;">"+"</span><span style="color:red;">)</span> <span>AssocLeft</span> <span style="color:red;">]</span>
<span>&gt;</span>             <span style="color:red;">]</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parsePolytype</span> <span style="color:red;">::</span> <span>Parser</span> <span>Polytype</span>
<span>&gt;</span> <span>parsePolytype</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>pty</span><span style="color:red;">@</span><span style="color:red;">(</span><span>Forall</span> <span>xs</span> <span>ty</span><span style="color:red;">)</span> <span style="color:red;">&lt;-</span> <span>parsePolytype'</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">let</span> <span>fvs</span> <span style="color:red;">::</span> <span>Set</span> <span>Var</span>
<span>&gt;</span>       <span>fvs</span> <span style="color:red;">=</span> <span>flip</span> <span>cata</span> <span>ty</span> <span>$</span> <span style="color:red;">\</span><span style="color:blue;font-weight:bold;">case</span>
<span>&gt;</span>         <span>TyVarF</span> <span>x</span>       <span style="color:red;">-&gt;</span> <span>S.singleton</span> <span>x</span>
<span>&gt;</span>         <span>TyNatF</span>         <span style="color:red;">-&gt;</span> <span>S.empty</span>
<span>&gt;</span>         <span>TyFunF</span> <span>xs1</span> <span>xs2</span> <span style="color:red;">-&gt;</span> <span>xs1</span> <span>`S.union`</span> <span>xs2</span>
<span>&gt;</span>       <span>unbound</span> <span style="color:red;">=</span> <span>fvs</span> <span>\\</span> <span>S.fromList</span> <span>xs</span>
<span>&gt;</span>   <span>unless</span> <span style="color:red;">(</span><span>S.null</span> <span>unbound</span><span style="color:red;">)</span> <span>$</span> <span>fail</span> <span>$</span> <span style="color:teal;">"Unbound type variables: "</span> <span>++</span> <span>unwords</span> <span style="color:red;">(</span><span>S.toList</span> <span>unbound</span><span style="color:red;">)</span>
<span>&gt;</span>   <span>return</span> <span>pty</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parsePolytype'</span> <span style="color:red;">::</span> <span>Parser</span> <span>Polytype</span>
<span>&gt;</span> <span>parsePolytype'</span> <span style="color:red;">=</span>
<span>&gt;</span>   <span>Forall</span> <span>&lt;$&gt;</span> <span style="color:red;">(</span><span>fromMaybe</span> <span>[]</span> <span>&lt;$&gt;</span> <span>optionMaybe</span> <span style="color:red;">(</span><span>reserved</span> <span style="color:teal;">"forall"</span> <span>*&gt;</span> <span>many</span> <span>identifier</span> <span>&lt;*</span> <span>symbol</span> <span style="color:teal;">"."</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span>           <span>&lt;*&gt;</span> <span>parseType</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parseTypeAtom</span> <span style="color:red;">::</span> <span>Parser</span> <span>Type</span>
<span>&gt;</span> <span>parseTypeAtom</span> <span style="color:red;">=</span>
<span>&gt;</span>   <span style="color:red;">(</span><span>TyNat</span> <span>&lt;$</span> <span>reserved</span> <span style="color:teal;">"nat"</span><span style="color:red;">)</span> <span>&lt;|&gt;</span> <span style="color:red;">(</span><span>TyVar</span> <span>&lt;$&gt;</span> <span>identifier</span><span style="color:red;">)</span> <span>&lt;|&gt;</span> <span>parens</span> <span>parseType</span>
<span>&gt;</span> 
<span>&gt;</span> <span>parseType</span> <span style="color:red;">::</span> <span>Parser</span> <span>Type</span>
<span>&gt;</span> <span>parseType</span> <span style="color:red;">=</span> <span>buildExpressionParser</span> <span>table</span> <span>parseTypeAtom</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>     <span>table</span> <span style="color:red;">=</span> <span style="color:red;">[</span> <span style="color:red;">[</span> <span>Infix</span> <span style="color:red;">(</span><span>TyFun</span> <span>&lt;$</span> <span>symbol</span> <span style="color:teal;">"-&gt;"</span><span style="color:red;">)</span> <span>AssocRight</span> <span style="color:red;">]</span> <span style="color:red;">]</span>
<span>&gt;</span> 
<span>&gt;</span> <span>expr</span> <span style="color:red;">::</span> <span>Parser</span> <span>Expr</span>
<span>&gt;</span> <span>expr</span> <span style="color:red;">=</span> <span>spaces</span> <span>*&gt;</span> <span>parseExpr</span> <span>&lt;*</span> <span>eof</span>
</code></pre>
<h2 id="pretty-printing">Pretty printing</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">type</span> <span>Prec</span> <span style="color:red;">=</span> <span>Int</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">class</span> <span>Pretty</span> <span>p</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">::</span> <span>p</span> <span style="color:red;">-&gt;</span> <span>String</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">=</span> <span>prettyPrec</span> <span class="hs-num">0</span>
<span>&gt;</span> 
<span>&gt;</span>   <span>prettyPrec</span> <span style="color:red;">::</span> <span>Prec</span> <span style="color:red;">-&gt;</span> <span>p</span> <span style="color:red;">-&gt;</span> <span>String</span>
<span>&gt;</span>   <span>prettyPrec</span> <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">=</span> <span>pretty</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span style="color:red;">(</span><span>t</span> <span style="color:red;">(</span><span>Fix</span> <span>t</span><span style="color:red;">)</span><span style="color:red;">)</span> <span style="color:red;">=&gt;</span> <span>Pretty</span> <span style="color:red;">(</span><span>Fix</span> <span>t</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>prettyPrec</span> <span>p</span> <span style="color:red;">=</span> <span>prettyPrec</span> <span>p</span> <span>.</span> <span>unFix</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span>t</span> <span style="color:red;">=&gt;</span> <span>Pretty</span> <span style="color:red;">(</span><span>TypeF</span> <span>t</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>prettyPrec</span> <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">(</span><span>TyVarF</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>v</span>
<span>&gt;</span>   <span>prettyPrec</span> <span style="color:blue;font-weight:bold;">_</span> <span>TyNatF</span> <span style="color:red;">=</span> <span style="color:teal;">"nat"</span>
<span>&gt;</span>   <span>prettyPrec</span> <span>p</span> <span style="color:red;">(</span><span>TyFunF</span> <span>ty1</span> <span>ty2</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>     <span>mparens</span> <span style="color:red;">(</span><span>p</span> <span>&gt;</span> <span class="hs-num">0</span><span style="color:red;">)</span> <span>$</span> <span>prettyPrec</span> <span class="hs-num">1</span> <span>ty1</span> <span>++</span> <span style="color:teal;">" -&gt; "</span> <span>++</span> <span>prettyPrec</span> <span class="hs-num">0</span> <span>ty2</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span style="color:red;">(</span><span>Pretty</span> <span style="color:red;">(</span><span>t</span> <span style="color:red;">(</span><span>UTerm</span> <span>t</span> <span>v</span><span style="color:red;">)</span><span style="color:red;">)</span><span style="color:red;">,</span> <span>Pretty</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">=&gt;</span> <span>Pretty</span> <span style="color:red;">(</span><span>UTerm</span> <span>t</span> <span>v</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>UTerm</span> <span>t</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>pretty</span> <span>t</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>UVar</span> <span>v</span><span style="color:red;">)</span>  <span style="color:red;">=</span> <span>pretty</span> <span>v</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span>Polytype</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>Forall</span> <span>[]</span> <span>t</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>pretty</span> <span>t</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>Forall</span> <span>xs</span> <span>t</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>unwords</span> <span style="color:red;">(</span><span style="color:teal;">"forall"</span> <span>:</span> <span>xs</span><span style="color:red;">)</span> <span>++</span> <span style="color:teal;">". "</span> <span>++</span> <span>pretty</span> <span>t</span>
<span>&gt;</span> 
<span>&gt;</span> <span>mparens</span> <span style="color:red;">::</span> <span>Bool</span> <span style="color:red;">-&gt;</span> <span>String</span> <span style="color:red;">-&gt;</span> <span>String</span>
<span>&gt;</span> <span>mparens</span> <span>True</span>  <span style="color:red;">=</span> <span style="color:red;">(</span><span style="color:teal;">"("</span><span>++</span><span style="color:red;">)</span> <span>.</span> <span style="color:red;">(</span><span>++</span><span style="color:teal;">")"</span><span style="color:red;">)</span>
<span>&gt;</span> <span>mparens</span> <span>False</span> <span style="color:red;">=</span> <span>id</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span>Expr</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>prettyPrec</span> <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">(</span><span>EVar</span> <span>x</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>x</span>
<span>&gt;</span>   <span>prettyPrec</span> <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">(</span><span>EInt</span> <span>i</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>show</span> <span>i</span>
<span>&gt;</span>   <span>prettyPrec</span> <span>p</span> <span style="color:red;">(</span><span>EPlus</span> <span>e1</span> <span>e2</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>     <span>mparens</span> <span style="color:red;">(</span><span>p</span><span>&gt;</span><span class="hs-num">1</span><span style="color:red;">)</span> <span>$</span>
<span>&gt;</span>       <span>prettyPrec</span> <span class="hs-num">1</span> <span>e1</span> <span>++</span> <span style="color:teal;">" + "</span> <span>++</span> <span>prettyPrec</span> <span class="hs-num">2</span> <span>e2</span>
<span>&gt;</span>   <span>prettyPrec</span> <span>p</span> <span style="color:red;">(</span><span>ELam</span> <span>x</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>     <span>mparens</span> <span style="color:red;">(</span><span>p</span><span>&gt;</span><span class="hs-num">0</span><span style="color:red;">)</span> <span>$</span>
<span>&gt;</span>       <span style="color:teal;">"\\"</span> <span>++</span> <span>x</span> <span>++</span> <span style="color:teal;">". "</span> <span>++</span> <span>prettyPrec</span> <span class="hs-num">0</span> <span>body</span>
<span>&gt;</span>   <span>prettyPrec</span> <span>p</span> <span style="color:red;">(</span><span>ELet</span> <span>x</span> <span>mty</span> <span>xdef</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>     <span>mparens</span> <span style="color:red;">(</span><span>p</span><span>&gt;</span><span class="hs-num">0</span><span style="color:red;">)</span> <span>$</span>
<span>&gt;</span>       <span style="color:teal;">"let "</span> <span>++</span> <span>x</span> <span>++</span> <span>maybe</span> <span style="color:teal;">""</span> <span style="color:red;">(</span><span style="color:red;">\</span><span>ty</span> <span style="color:red;">-&gt;</span> <span style="color:teal;">" : "</span> <span>++</span> <span>pretty</span> <span>ty</span><span style="color:red;">)</span> <span>mty</span>
<span>&gt;</span>             <span>++</span> <span style="color:teal;">" = "</span> <span>++</span> <span>prettyPrec</span> <span class="hs-num">0</span> <span>xdef</span>
<span>&gt;</span>             <span>++</span> <span style="color:teal;">" in "</span> <span>++</span> <span>prettyPrec</span> <span class="hs-num">0</span> <span>body</span>
<span>&gt;</span>   <span>prettyPrec</span> <span>p</span> <span style="color:red;">(</span><span>EApp</span> <span>e1</span> <span>e2</span><span style="color:red;">)</span> <span style="color:red;">=</span>
<span>&gt;</span>     <span>mparens</span> <span style="color:red;">(</span><span>p</span><span>&gt;</span><span class="hs-num">2</span><span style="color:red;">)</span> <span>$</span>
<span>&gt;</span>       <span>prettyPrec</span> <span class="hs-num">2</span> <span>e1</span> <span>++</span> <span style="color:teal;">" "</span> <span>++</span> <span>prettyPrec</span> <span class="hs-num">3</span> <span>e2</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span>IntVar</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">=</span> <span>mkVarName</span> <span style="color:teal;">"u"</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span>TypeError</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>UnboundVar</span> <span>x</span><span style="color:red;">)</span>     <span style="color:red;">=</span> <span>printf</span> <span style="color:teal;">"Unbound variable %s"</span> <span>x</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>Infinite</span> <span>x</span> <span>ty</span><span style="color:red;">)</span>    <span style="color:red;">=</span> <span>printf</span> <span style="color:teal;">"Infinite type %s = %s"</span> <span style="color:red;">(</span><span>pretty</span> <span>x</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>pretty</span> <span>ty</span><span style="color:red;">)</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>Mismatch</span> <span>ty1</span> <span>ty2</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>printf</span> <span style="color:teal;">"Can't unify %s and %s"</span> <span style="color:red;">(</span><span>pretty</span> <span>ty1</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>pretty</span> <span>ty2</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span>Value</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>VInt</span> <span>n</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>show</span> <span>n</span>
<span>&gt;</span>   <span>pretty</span> <span style="color:red;">(</span><span>VClo</span> <span>x</span> <span>body</span> <span>env</span><span style="color:red;">)</span>
<span>&gt;</span>     <span style="color:red;">=</span> <span>printf</span> <span style="color:teal;">"&lt;%s: %s %s&gt;"</span>
<span>&gt;</span>       <span>x</span> <span style="color:red;">(</span><span>pretty</span> <span>body</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>pretty</span> <span>env</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Pretty</span> <span>Env</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>pretty</span> <span>env</span> <span style="color:red;">=</span> <span style="color:teal;">"["</span> <span>++</span> <span>intercalate</span> <span style="color:teal;">", "</span> <span>bindings</span> <span>++</span> <span style="color:teal;">"]"</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>       <span>bindings</span> <span style="color:red;">=</span> <span>map</span> <span>prettyBinding</span> <span style="color:red;">(</span><span>M.assocs</span> <span>env</span><span style="color:red;">)</span>
<span>&gt;</span>       <span>prettyBinding</span> <span style="color:red;">(</span><span>x</span><span style="color:red;">,</span> <span>v</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>x</span> <span>++</span> <span style="color:teal;">" -&gt; "</span> <span>++</span> <span>pretty</span> <span>v</span>
</code></pre>

