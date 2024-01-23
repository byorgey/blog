---
title: A word to the wise
published: 2007-12-19T12:25:26Z
categories: grad school,haskell
tags: ghci,Ord
---

If you only implement the less than operator in a custom Ord instance (on the theory that "I know I only need to implement one operation to get defaults for the others, and less than makes sense, since you can get anything using less than and equals"), the compiler gives zero warnings (even with -Wall) and trying to compare anything will send your program into nasty infinite recursion.  It turns out that you have to implement less than or equal to (or the 'compare' function), not less than.  Says so in the docs for Ord, of course, but... sigh.

In related news, the new ghci debugger is quite helpful. =)

And in unrelated news, I'm finally done with grad school and fellowship applications!!

