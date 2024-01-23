---
title: Data structure challenge: finding the rightmost empty slot
published: 2020-03-23T11:53:08Z
categories: data structures
tags: challenge,data,empty,full,log,rightmost,slot,structure
---

<p>Suppose we have a sequence of <em>slots</em> indexed from 1 to $latex n$. Each slot can be either <em>empty</em> or <em>full</em>, and all start out empty. We want to repeatedly do the following operation:</p>
<ul>
<li>Given an index $latex i$, find the rightmost empty slot at or before index $latex i$, and mark it full.</li>
</ul>
<p>We can also think of this in terms of two more fundamental operations:</p>
<ul>
<li>Mark a given index $latex i$ as full.</li>
<li>Given an index $latex i$, find the greatest index $latex j$ such that $latex j \leq i$ and $latex j$ is empty (or $latex 0$ if there is no such $latex j$).</li>
</ul>
<p>The simplest possible approach would be to use an array of booleans; then marking a slot full is trivial, and finding the rightmost empty slot before index $latex i$ can be done with a linear scan leftwards from $latex i$. But the challenge is this:</p>
<p><em>Can you think of a data structure to support both operations in $latex O(\lg n)$ time or better?</em></p>
<p>You can think of this in either functional or imperative terms. I know of two solutions, which I’ll share in a subsequent post, but I’m curious to see what people will come up with.</p>
<p>Note that in my scenario, slots never become empty again after becoming full. As an extra challenge, what if we relax this to allow setting slots arbitrarily?</p>

