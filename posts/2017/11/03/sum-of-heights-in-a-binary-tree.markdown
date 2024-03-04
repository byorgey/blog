---
katex: true
title: 'Sum of heights in a binary tree'
published: 2017-11-03T15:06:40Z
categories: math,teaching
tags: amortized,analysis,binary,build,heap,height,linear,log
---

<p><em>Executive summary: every year when teaching data structures I always forget how to analyze the cost of building a binary heap, which amounts to summing the heights of all the nodes in a full binary tree. So I’m writing down the (lovely) proof here in the hopes that I will remember it next time.</em></p>
<p>Suppose you have a full binary tree and you do an operation on every node, where the cost of the operation is proportional to the height of that node. That is, the cost for each of the $n/2$ leaves is $0$, for each of the $n/4$ nodes in the next level up the cost is $1$, and so on. We can visualize the scenario like this:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/11/163d289d2af7bf4f.png" /></p>
</div>
<p>As a function of the total number of nodes $n$, how expensive is this? We can see that $O(n \lg n)$ is an upper bound, since there are $n$ nodes and the height of each node is at most $\lg n$. But it seems like it might actually be faster than this in reality, since, intuitively, <em>most</em> of the nodes have a height which is much smaller than $\lg n$.</p>
<p>(One specific motivation for this scenario is that we can build a <a href="https://en.wikipedia.org/wiki/binary%20heap">binary heap</a> from an arbitrary set of data by looping over the nodes from the bottom up and calling <code>reheapDown</code> on each; in the worst case <code>reheapDown</code> takes time proportional to the height of the node, as in this scenario. But it doesn’t matter if you don’t know about binary heaps.)</p>
<p>Let’s take the same tree and put a dollar at every node, for a total of $\$n$:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/11/e06819c343da6ed3.png" /></p>
</div>
<p>Now imagine sliding all the money as far up and to the right as it will go. That is, we take each dollar, and keep moving it up as long as it is a left child. As soon as we reach a node which is a right child we stop. The tree ends up looking like this:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/11/e3505964e049eb59.png" /></p>
</div>
<p>Now take each pile of money and move it up one step to its parent, except the money at the root of the tree, which you can put in your pocket.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/11/3571afa7a86984a0.png" /></p>
</div>
<p>And voilà! We now have exactly enough money at each node to pay for the cost of the operations, and we even have a bit left over (which we can use to buy coffee). But we started with $\$n$ and only shuffled money around; this shows that the total cost is actually $O(n)$.</p>
<p>Exercise for the reader: what does this have to do with the number of bit flips needed to count from $1$ to $n$ with a binary counter?</p>

