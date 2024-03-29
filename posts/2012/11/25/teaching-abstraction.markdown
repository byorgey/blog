---
katex: true
title: 'Teaching abstraction'
published: 2012-11-25T19:24:22Z
categories: teaching
tags: abstraction,applicative,monad,pedagogy
---

<p>I’m just beginning to prepare for the third incarnation of <a href="http://www.cis.upenn.edu/~cis194/">CIS 194, Introduction to Haskell</a> in the spring. It’s occasioned some general thoughts on <em>teaching abstraction</em> which seemed worth writing down.</p>
<p>Abstractions, of course, are everywhere in CS. By <em>abstraction</em> I mean anything with multiple “levels”, where upper levels hide details of lower levels. An operating system abstracts over the details of hardware; objects abstract over their internal state; functions abstract over their implementation; and so on. Let me just state up front my main thesis:</p>
<p><em>Students should be explicitly taught to think on multiple levels of abstraction.</em></p>
<p>Stated like that, perhaps it seems obvious; allow me to elaborate.</p>
<p>On the face of it, when learning about some abstraction, there are two things one can learn. One can learn <em>how the abstraction is implemented</em> (that is, the details of the lower level); and one can also learn <em>how to use the abstraction</em> (that is, how to work with the upper level while ignoring the details of the lower level).</p>
<p>In many cases, students already know one of the levels and learn about the other. For example, once they get around to learning about how compilers work, students already know how to write programs. Or perhaps they learn both levels as part of the same course, but at different times: for example, they might first learn about logic gates, and then later move on to talking about latches and adders.</p>
<p>It’s when teaching <em>both levels at once</em> that one must be extremely careful. Such a situation might arise, for example, when teaching a concept that is completely new or difficult to motivate—since it’s best to start with the concrete and move to the abstract, one might wish to begin with the lower level, but referencing the upper level is necessary to motivate the lower level in the first place. The problem with such a situation is that it is really easy for students to get the levels confused.</p>
<p>In particular, I have in mind the <code>Applicative</code> functor abstraction (and <code>Monad</code> as well). The last time teaching CIS 194, I didn’t do a very good job with this. I gave my students a homework assignment which had them first <em>implement</em> an <code>Applicative</code> interface to parser combinators, and then <em>use</em> it to write some parsers. Most students missed the point, however, and kept on using low-level implementation details when writing their parsers, instead of working in terms of the <code>Applicative</code> interface.</p>
<p>It’s tempting to just say “don’t do that”, <em>i.e.</em> don’t confuse students by teaching multiple levels at once! But I think the situation actually presents a great opportunity. In the “real world”, one seldom has the luxury of thinking on only one level at a time. Being able to explicitly switch between levels, and keep multiple levels straight, is an important real-world skill. I think the real solution is to <em>explicitly teach students to think on multiple levels of abstraction</em>: that is, be explicit about the fact that there <em>are</em> multiple levels, and teach them to consider carefully at each point which level they are thinking on, and why. I plan to do this in the spring and will report back on how it goes!</p>

