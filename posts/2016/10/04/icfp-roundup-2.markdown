---
title: 'ICFP roundup'
published: 2016-10-05T03:36:28Z
categories: haskell
tags: diagrams,ICFP
---

<p><a href="http://icfpconference.org/icfp2016/">ICFP 2016 in Nara, Japan</a> was a blast. Here are a few of my recollections.</p>
<h2 id="the-place">The Place</h2>
<p>Although I was a coathor on an ICFP paper in 2011, when it was in Tokyo, I did not go since my son was born the same week. So this was my first time in Japan, or anywhere in Asia, for that matter. (Of course, this time I missed my son’s fifth birthday…)</p>
<p>I’ve been to Europe multiple times, and although it is definitely foreign, the culture is similar enough that I feel like I basically know how to behave. I did <em>not</em> feel that way in Japan. I’m pretty sure I was constantly being offensive without realizing it, but most of the time people were polite and accommodating.</p>
<p>…EXCEPT for that one time I was sitting in a chair chatting with folks during a break between sessions, with my feet up on a (low, plain) table, and an old Japanese guy WHACKED his walking stick on the table and shouted angrily at me in Japanese. That sure got my adrenaline going. Apparently putting your feet on the table is a big no-no, lesson learned.</p>
<p>The food was amazing even though I didn’t know what half of it was. I was grateful that I (a) am not vegetarian, (b) know how to use chopsticks decently well, and (c) am an adventurous eater. If any one of those were otherwise, things might have been more difficult!</p>
<p>On my last day in Japan I had the whole morning before I needed to head to the airport, so Ryan Yates and I wandered around Nara and saw a bunch of temples, climbed the hill, and such. It’s a stunningly beautiful place with a rich history.</p>
<h2 id="the-people">The People</h2>
<p>As usual, it’s all about the people. I enjoyed meeting some new people, including (but not limited to):</p>
<ul>
<li>Pablo Buiras and Marco Vassena were my hotel breakfast buddies, it was fun getting to know them a bit.</li>
<li>I finally met Dominic Orchard, though I feel like I’ve known his name and known about some of his work for a while.</li>
<li>I don’t think I had met Max New before but we had a nice chat about the Scheme enumerations library he helped develop and combinatorial species. I hope to be able to follow up that line of inquiry.</li>
<li>As promised, I met everyone who commented on my blog post, including Jürgen Peters (unfortunately we did not get a chance to play go), Andrey Mokhov (who nerd-sniped me with a cool semiring-ish thing with some extra structure — perhaps that will be another blog post), and Jay McCarthy (whom I had actually met before, but we had some nice chats, including one in the airport while waiting for our flight to LAX).</li>
<li>I don’t think I had met José Manuel Calderón Trilla before; we had a great conversation over a meal together (along with Ryan Yates) in the Osaka airport while waiting for our flights.</li>
<li>I met Diogenes Nunez, who went to my alma mater Williams College. When I taught at Williams a couple years ago I’m pretty sure I heard Diogenes mentioned by the other faculty, so it was fun to get to meet him.</li>
<li>Last but certainly not least, I met my coauthor, Piyush Kurur. We collaborated on a paper through the magic of the Internet (Github in particular), and I actually met him in person for the first time just hours before he presented our paper!</li>
</ul>
<p>My student Ollie Kwizera came for PLMW—it was fun having him there. I only crossed paths with him three or four times, but I think that was all for the best, since he made his own friends and had his own experiences.</p>
<p>Other people who I enjoyed seeing and remember having interesting conversations with include (but I am probably forgetting someone!) Michael Adams, Daniel Bergey, Jan Bracker, Joachim Breitner, David Christiansen, David Darais, Stephen Dolan, Richard Eisenberg, Kenny Foner, Marco Gaboardi, Jeremy Gibbons, John Hughes, David Janin, Neel Krishnaswami, Dan Licata, Andres Löh, Simon Marlow, Tom Murphy, Peter-Michael Osera, Jennifer Paykin, Simon Peyton Jones, Ryan Scott, Mary Sheeran, Mike Sperber, Luite Stegeman, Wouter Swierstra, David Terei, Ryan Trinkle, Tarmo Uustalu, Stephanie Weirich, Nick Wu, Edward Yang, and Ryan Yates. My apologies if I forgot you, just remind me and I’ll add you to the list! I’m amazed and grateful I get to know all these cool people.</p>
<h2 id="the-content">The Content</h2>
<p>Here are just a few of my favorite talks:</p>
<ul>
<li><p>I’m a sucker for anything involving geometry and/or random testing and/or pretty pictures, and Ilya Sergey’s talk <a href="https://www.youtube.com/watch?v=P_KPjQ1p60k">Growing and Shrinking Polygons for Random testing of Computational Geometry</a> had them all. In my experience, doing effective random testing in any domain beyond basic functions usually requires some interesting domain-specific insights, and Ilya had some cool insights about ways to generate and shrink polygons in ways that were much more likely to generate small counterexamples for computational geometry algorithms.</p></li>
<li><p><a href="http://www.idris-lang.org/">Idris</a> gets more impressive by the day, and I always enjoy <a href="https://www.youtube.com/watch?v=pqFgYCdiYz4">David Christiansen’s talks</a>.</p></li>
<li><p>Sandra Dylus gave a fun talk, <a href="https://youtu.be/vV3jqTxJ9Wc">All Sorts of Permutations</a>, with the cute observation that a sorting algorithm equipped with a nondeterministic comparison operator generates permutations (though it goes deeper than that). During the question period someone asked whether there is a way to generate all partitions, and someone sitting next to me suggested using the <code>group</code> function—and indeed, I think this works. I wonder what other sorts of combinatorial objects can be enumerated by this method. In particular I wonder if quicksort with nondeterministic comparisons can be adapted to generate not just all permutations, but all binary trees.</p></li>
<li><p>I greatly enjoyed TyDe, especially Jeremy Gibbons’ talk on APLicative Programming with Naperian Functors (I don’t think the video is online yet, if there is one). I’ll be serving as co-chair of the TyDe program committee next year, so start thinking about what you would like to submit!</p></li>
<li><p>There were also some fun talks at FARM, for example, Jay McCarthy’s talk on Bithoven. But I don’t think the FARM videos are uploaded yet. Speaking of FARM, the performance evening was incredible. It will be hard to live up to next year.</p></li>
</ul>

