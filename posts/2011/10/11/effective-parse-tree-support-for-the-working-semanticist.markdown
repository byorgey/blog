---
title: Effective parse tree support for the working semanticist
published: 2011-10-11T19:04:25Z
categories: haskell,projects
tags: ambiguous,grammar,Ott,parse,pretty,tree
---

If, like me, you spend large amounts of time designing, discussing, and proving things about programming languages and formal calculi, chances are you have used <a href="http://www.cl.cam.ac.uk/~pes20/ott/" title="Ott">Ott</a> (and if you do, but you haven't, you are really missing out).  You write down a formal system once, in a special format that Ott understands, and Ott typechecks it and can automatically generate nice LaTeX as well as code for proof assistants like Coq or Isabelle.

However, anyone who has worked with Ott is familiar with is the dreaded ambiguous parse error.  Quite often Ott grammars end up being ambiguous, and Ott needs some guidance to know which parse you want.  However, the error messages are horrendous: Ott spews forth several massive parse trees in a format requiring formidable patience to read.  Finding the slight differences between two such parse trees is an exercise in seizure-inducing tedium.

I got sick of this so I wrote a little tool to help, which is now available on Hackage: <a href="http://hackage.haskell.org/package/ottparse%2Dpretty" title="ottparse-pretty">ottparse-pretty</a>.  You paste in an Ott parse tree and it shows it to you in a nicely formatted tree view with a bunch of useless cruft removed.  For example, it turns this:

<code>
(St_node :formula:formula_judgement: (Ste_st (St_node :judgement:judgement_Jtype: (Ste_st 
(St_node :Jtype:coerce: (Ste_st (St_nonterm G)) (Ste_st (St_nonterm g)) (Ste_st (St_node 
:s:T_MultiTypeApp: (Ste_st (St_node :s:T_EqTy: (Ste_st (St_node :s:T_MultiTypeApp: (Ste_st 
(St_node :s:T_MultiKindApp: (Ste_st (St_node :s:T_Cons: (Ste_st (St_nonterm H)))) (Ste_st 
(St_nonterm k)))) (Ste_st (St_nonterm t)))) (Ste_st (St_node :s:T_MultiKindApp: (Ste_st 
(St_node :s:T_Cons: (Ste_st (St_nonterm H)))) (Ste_st (St_nonterm k)))))) (Ste_st (St_nonterm 
t')))))))))
</code>

into this:

<code><pre>
formula_judgement
|
`- judgement_Jtype
   |
   `- coerce
      |
      +- G
      |
      +- g
      |
      `- T_MultiTypeApp
         |
         +- T_EqTy
         |  |
         |  +- T_MultiTypeApp
         |  |  |
         |  |  +- T_MultiKindApp
         |  |  |  |
         |  |  |  +- T_Cons
         |  |  |  |  |
         |  |  |  |  `- H
         |  |  |  |
         |  |  |  `- k
         |  |  |
         |  |  `- t
         |  |
         |  `- T_MultiKindApp
         |     |
         |     +- T_Cons
         |     |  |
         |     |  `- H
         |     |
         |     `- k
         |
         `- t'
</pre></code>

It's pretty simplistic at the moment---I may add more features depending on feedback.  But it's been useful for me so it seemed worth packaging it up in case it's useful to anyone else.

