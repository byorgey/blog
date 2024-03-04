---
katex: true
title: 'Types for top-level definitions'
published: 2022-08-27T12:47:50Z
categories: projects
tags: context,definition,types
---

<p>I’ve come up with idea for a type system for first-class (global) definitions, which can serve as a very lightweight alternative to a proper module system. I’m posting it here in the hopes of getting some feedback and pointers to related work.</p>
<h2 id="commands-and-expressions">Commands and expressions</h2>
<p>The programming language of <a href="https://github.com/swarm-game/swarm/">Swarm</a> (for lack of a better term I will hereafter refer to it as <em>Swarmlang</em>) has a bunch of imperative commands, and standard monadic sequencing constructs. For example,</p>
<pre><code>move; move</code></pre>
<p>does two <code>move</code> commands in sequence, and</p>
<pre><code>thing &lt;- grab; give friend thing</code></pre>
<p>first executes <code>grab</code>, binding the variable <code>thing</code> to the result, then executes <code>give friend thing</code>. Of course, there is also a rich language of pure expressions, with things like arithmetic, strings, lambdas and function application, pairs, sums, and so on.</p>
<p>Some languages make a syntactic distinction between statements and expressions, but Swarmlang does not: everything is an expression, and some expressions happen to have a command type. If <code>t</code> is a type, then <code>cmd t</code> is the type of an imperative command which, when executed, can have some effects and then returns a result of type <code>t</code>. (Of course this should feel very familiar to Haskell programmers; <code>cmd</code> has many similarities to <code>IO</code>.) This approach makes many things simpler and means that commands are first-class values.</p>
<h2 id="typechecking-definitions">Typechecking definitions</h2>
<p>Swarmlang has <em>definitions</em>, which are just expressions with a command type. If <code>e</code> is an expression, then</p>
<pre><code>def x = e end</code></pre>
<p>has type <code>cmd ()</code>. When executed, it should have the effect of binding the name <code>x</code> to the expression <code>e</code>, and bringing <code>x</code> into scope for all subsequent commands. Thus, it is valid to sequence this first definition with a second definition that mentions <code>x</code>, like so:</p>
<pre><code>def x = e end;
def y = foo bar x end</code></pre>
<p>Of course, this means that while typechecking the definition of <code>y</code>, we must be able to look up the type of <code>x</code>. However, the type of the first <code>def</code> command is simply <code>cmd ()</code>, which does not tell us anything about <code>x</code> or its type. Normally, the typing rule for sequencing of commands would be something like</p>
<div style="text-align:center;">
<p>$\displaystyle \frac{\Gamma \vdash c_1 : \mathrm{cmd}\; \tau_1 \qquad \Gamma \vdash c_2 : \mathrm{cmd}\; \tau_2}{\Gamma \vdash c_1 ; c_2 : \mathrm{cmd}\;\tau_2}$</p>
</div>
<p>but this does not work for <code>def</code> commands, since it does not take into account the new names brought into scope. Up until now, I have dealt with this in a somewhat ad-hoc manner, with some special typechecking rules for <code>def</code> and some ad-hoc restrictions to ensure that <code>def</code> can only syntactically show up at the top level. However, I would <a href="https://github.com/swarm-game/swarm/issues/636">really like to put everything on a more solid theoretical basis</a> (which will hopefully simplify the code as well).</p>
<h2 id="decorating-command-types">Decorating command types</h2>
<p>The basic idea is to decorate the $\mathrm{cmd}$ type with extra information about names bound by definitions. As usual, let $\Gamma$ denote a generic <em>context</em>, that is, a finite mapping from variable names to their types. Then we extend the <code>cmd</code> type by adding a context to it:</p>
<div style="text-align:center;">
<p>$\mathrm{cmd}\; \tau \Rightarrow \Gamma$</p>
</div>
<p>is the type of a command which yields a result of type $\tau$ <em>and produces global bindings for some names whose types are recorded in $\Gamma$</em>. (Of course, we can continue to use $\mathrm{cmd}\; \tau$ as an abbreviation for $\mathrm{cmd}\; \tau \Rightarrow \varnothing$.) So, for example, <code>def x = 3 end</code> no longer has type $\mathrm{cmd}\; ()$, but rather something like $\mathrm{cmd}\; () \Rightarrow \{x : \mathrm{int}\}$, representing the fact that although <code>def x = 3 end</code> does not result in an interesting value, it does bind a name, <code>x</code>, whose type is <code>int</code>.</p>
<p>This is slightly unusual in the fact that types and contexts are now mutually recursive, but that doesn’t seem like a big problem. We can now write down a proper typing rule for sequencing that takes definitions into account, something like this:</p>
<div style="text-align:center;">
<p>$\displaystyle \frac{\Gamma \vdash c_1 : \mathrm{cmd} \; \tau_1 \Rightarrow \Gamma_1 \qquad \Gamma, \Gamma_1 \vdash c_2 : \mathrm{cmd} \; \tau_2 \Rightarrow \Gamma_2}{\Gamma \vdash c_1 ; c_2 : \mathrm{cmd} \; \tau_2 \Rightarrow \Gamma, \Gamma_1, \Gamma_2}$</p>
</div>
<p>And of course the typing rule for <code>def</code> looks like this:</p>
<div style="text-align:center;">
<p>$\displaystyle \frac{\Gamma \vdash e : \tau}{\Gamma \vdash \texttt{def}\; x = e\; \texttt{end} : \mathrm{cmd}\; () \Rightarrow \{x : \tau\}}$</p>
</div>
<p>These rules together can now correctly typecheck an expression like</p>
<pre><code>def x = 3 end;
def y = 2 + x end</code></pre>
<p>where the second definition refers to the name defined by the first. The whole thing would end up having type $\mathrm{cmd}\; () \Rightarrow \{ x : \mathrm{int}, y : \mathrm{int} \}$.</p>
<h2 id="with-polymorphism">…with polymorphism?</h2>
<p>All this seems straightforward with only first-order types, as in my example typing rules above. But once you add parametric polymorphism my brain starts to hurt. Clearly, the context associated to a command type could bind variables to polytypes. For example, <code>def id = \x.x end</code> has type $\mathrm{cmd}\; () \Rightarrow \{id : \forall \alpha. \alpha \to \alpha\}$. But should the context associated to a command type <em>always</em> contain polytypes, or only when the command type is itself a polytype? In other words, how do we deal with the associated contexts in the monotypes that show up during type inference? And what would it mean to <em>unify</em> two command types with their contexts (and would that ever even be necessary)? I hope it’s actually simple and I just need to think about it some more, but I haven’t wrapped my brain around it yet.</p>
<h2 id="ideas-and-pointers-welcome">Ideas and pointers welcome!</h2>
<p>I’d be very happy to hear anyone’s ideas, or (especially) pointers to published work that seems related or relevant! Feel free to comment either here, or <a href="https://github.com/swarm-game/swarm/issues/636">on the relevant github issue</a>.</p>

