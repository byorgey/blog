---
title: Conversations with a six-year-old on functional programming
published: 2018-05-06T19:19:00Z
categories: teaching
tags: children,function,game,input,machine,output
---

<p>My six-year-old son walked up to me yesterday. “What are you reading?”</p>
<p>At the time, I was reading part of <a href="http://www.janis-voigtlaender.eu/papers/TypesForProgrammingAndReasoning.pdf">Janis Voigtländer’s habilitation thesis</a>. Unsure where to even start, I decided to just answer straightforwardly: “I’m reading a very long story about free theorems.”</p>
<p>He persisted. “What are free theorems?”</p>
<p>Never one to shrink from a pedagogical challenge, I thought for a moment, then began: “Do you know what a <em>function</em> is?” He didn’t. “A function is like a machine where you put something in one end and something comes out the other end. For example, maybe you put a number in, and the number that is one bigger comes out. So if you put in three, four comes out, or if you put in six, seven comes out.” This clearly made sense to him, so I continued, “The <em>type</em> of a function machine tells you what kinds of things you put in and what kinds of things come out. So maybe you put a number in and get a number out. Or maybe you put in a <em>list</em> of numbers and get a number out.” He interrupted excitedly, “Or maybe you could put <em>words</em> in??” “Yes, exactly! Maybe you can put words in and get words out. Or maybe there is a function machine where you put <em>other function machines</em> in and get function machines out!” He gasped in astonishment at the idea of putting function machines into function machines.</p>
<p>“So,” I concluded, “a free theorem is when you can say something that is always true about a function machine if you <em>only</em> know its type, but you <em>don’t</em> know anything about what it does on the inside.” This seemed a bit beyond him (and to be fair, free theorems are only interesting when polymorphism is involved which I definitely didn’t want to go into). But the whole conversation had given me a different idea.</p>
<p>“Hey, I have a good idea for a game,” I said. “It’s called the function machine game. I will think of a function machine. You tell me things to put into the function machine, and I will tell you what comes out. Then you have to guess what the function machine does.” He immediately liked this game and it has been a huge hit; he wants to play it all the time. We played it while driving to a party yesterday, and we played it this morning while I was in the shower. So far, he has correctly guessed:</p>
<ul>
<li>$latex \lambda x.\, x + 1$</li>
<li>$latex \lambda x.\, x - 3$</li>
<li>$latex \lambda x.\, 10x$</li>
<li>$latex \lambda x.\, 2x$</li>
<li>$latex \lambda x.\, 6$</li>
<li>$latex \lambda w\!:\!\text{English noun}.\, \text{plural}(w)$</li>
<li>$latex \lambda x.\, 10 \lfloor x/10 \rfloor$</li>
</ul>
<p>I tried $latex \lambda x.\, \min(x+2,10)$ but that was a bit tough for him. I realized that in some cases he may understand intuitively what the function does but have trouble expressing it in words (this was also a problem with $latex \lambda x.\, 10 \lfloor x/10 \rfloor$), so we started using the obvious variant where once the guesser thinks they know what the function does, the players switch roles and the person who came up with function specifies some inputs in order to test whether the guesser is able to produce the correct outputs.</p>
<p>$latex \lambda x.\, 6$ was also surprisingly difficult for him to guess (though he did get it right eventually). I think he was just stuck on the idea of the function doing something arithmetical to the input, and was having trouble coming up with some sort of arithmetic procedure which would result in $latex 6$ no matter what you put in! It simply hadn’t occurred to him that the machine might <em>not care</em> about the input. (Interestingly, many students in my functional programming class this semester were also confused by constant functions when we were learning about the lambda calculus; they really wanted to substitute the input <em>somewhere</em> and were upset/confused by the fact that the bound variable did not occur in the body at all!)</p>
<p>After a few rounds of guessing my functions, he wanted to come up with his own functions for me to guess (as I knew he would). Sometimes his functions are great and sometimes they don’t make sense (usually because his idea of what the function does changes over time, which of course he, in all sincerity, denies), but it’s fun either way. And after he finally understood $latex \lambda x. \min(x+2, 10)$, he came up with his own function which was something like</p>
<p>$latex \lambda x:\mathbb{N}. \begin{cases} 10 - x &amp; x \leq 10 \\ 10 &amp; \text{otherwise} \end{cases}$</p>
<p>inspired, I think, by his kindergarten class where they were learning about pairs of numbers that added up to $latex 10$.</p>
<p>Definitely one of my better parenting days.</p>

