---
title: Signed sets and ballots, part 1
published: 2017-02-21T03:42:11Z
categories: combinatorics,math
tags: ballots,bijection,involution,sets,signed,species,virtual
---

<p>The other day, <a href="http://akc.is/">Anders Claesson</a> wrote a <a href="http://akc.is/blog/2017-02-18-Inverse-species-and-sign-reversing-involutions.html">very nice blog post</a> explaining a more combinatorial way to understand multiplicative inverses of virtual species (as opposed to the <a href="https://byorgey.wordpress.com/2017/02/10/virtual-species-suffice/">rather algebraic way I explained it in my previous post</a>). In the middle of his post he makes an offhanded assumption which I stubbornly refused to take at face value; after thinking about it for a while and discussing it with Anders, I’m very glad I did, because there’s definitely more going on here than meets the eye and it’s given me a lot of interesting things to think about.</p>
<p><a href="https://byorgey.wordpress.com/2013/01/07/the-algebra-of-species-primitives/">Recall</a> that $latex E$ denotes the species of <em>sets</em>, defined by $latex E[U] = \{U\}$, that is, the only $latex E$-structure on a given label set $latex U$ is the set of labels itself. Recall also that the <em>exponential generating function</em> of a species $latex F$ is given by</p>
<p><div style="text-align:center;">
$latex \displaystyle F(x) = \sum_{n \geq 0} f_n \frac{x^n}{n!}$
</div></p>
<p>where $latex f_n$ counts the number of labelled $latex F$-structures of size $latex n$. In the case of $latex E$, we have $latex f_n = 1$ for all $latex n$, so</p>
<p><div style="text-align:center;">
$latex \displaystyle E(x) = \sum_{n \geq 0} \frac{x^n}{n!} = e^x.$
</div></p>
<p>(This is why $latex E$ is such a good name for the species of sets—though in a fantastic coincidence, it seems to originally come from the French word for set, <em>ensemble</em>, rather than from the fact that $latex E(x) = e^x$ (though on the other hand calling it a “coincidence” is probably too strong, since Joyal must surely have picked the notation with the generating function already in mind!).)</p>
<p>Now, from <a href="https://byorgey.wordpress.com/2017/02/10/virtual-species-suffice/">my previous post</a> we know that</p>
<p><div style="text-align:center;">
$latex \displaystyle E^{-1} = (1 + E_+)^{-1} = \sum_{k \geq 0} (-1)^k (E_+)^k.$
</div></p>
<p>Let’s first consider $latex \sum_k (E_+)^k$ (without the $latex (-1)^k$). This means that we have, for some $latex k \geq 0$, a $latex k$-ary product of $latex E_+$ structures—in other words, a <em>list</em> of nonempty sets. This is the species of <em>ballots</em>, also known as <em>ordered set partitions</em>, and can also be written $latex L \circ E_+$. As an example, here is a ballot on the set of labels $latex \{1, \dots, 8\}$:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/c2e7cb4f515e8038.png" /></p>
</div>
<p>The order of the parts matters, so this is a different ballot:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/59ddb0d0198f3856.png" /></p>
</div>
<p>But the order of labels <em>within</em> each part doesn’t matter (since each part is a set). As another example, here is the complete collection of ballot structures on the labels $latex \{1,2,3\}$:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/8b59608fc2f5cd9e.png" /></p>
</div>
<p>We can see that there are 13 in total: six where the labels are each in their own separate part (corresponding to the six possible permutations of the labels); six where two labels share a part and the other label is a singleton part (corresponding to the three ways to choose the solitary label, times the two ways to order the parts); and one final ballot where all three labels are grouped in the same part. (As an exercise, can you verify that there are 75 different ballot structures on a set of four labels?)</p>
<p>Returning to $latex E^{-1} = \sum_k (-1)^k (E_+)^k$, we can see that it consists of <em>signed</em> ballots, where the sign of a ballot is the parity of its number of parts, that is, a ballot with $latex k$ parts has sign $latex (-1)^k$. The second half of <a href="http://akc.is/blog/2017-02-18-Inverse-species-and-sign-reversing-involutions.html">Anders’ post</a> gives a nice combinatorial proof that $latex E \cdot E^{-1} = 1$, via a sign-reversing involution: if we consider $latex E \cdot E^{-1}$-structures, <em>i.e.</em> pairs of sets and signed ballots, there is a natural<a href="#fn1" class="footnoteRef" id="fnref1"><sup>1</sup></a> way to pair them up, matching positive and negative structures so everything cancels (except in the case of the empty label set, which is why we get $latex 1$ instead of $latex 0$).</p>
<p>However, Anders is trying to do more than that. Note first that since multiplication of EGFs corresponds to multiplication of species, the EGF for $latex E^{-1}$ is of course $latex 1/e^x = e^{-x}$. But this ought to also be the EGF for the virtual species $latex E(-X)$, and the rest of his post hinges on identifying $latex E(-X)$ and $latex E^{-1}$. As Anders and I discovered, however, this is precisely the point where it is worth being more careful.</p>
<p>First of all, what <em>is</em> $latex E(-X)$? Intuitively, an $latex E(-X)$ structure consists of a set of <em>negative</em> atoms; since each set can be thought of as an (unordered) product of atoms, the whole set acquires a sign given by the parity of the number of atoms. In other words, intuitively it seems that $latex E(-X)$ should be the species of <em>signed sets</em>, where an even-sized set is considered positive and an odd-sized set negative. That is,</p>
<p><div style="text-align:center;">
$latex \displaystyle E(-X) = \sum_{n \geq 0} (-1)^n E_n,$
</div></p>
<p>where $latex E_n$ denotes the species of sets of size exactly $latex n$. As a sanity check, this makes sense as an EGF equation too, since the EGF of $latex E_n$ is just $latex x^n/n!$ and indeed</p>
<p><div style="text-align:center;">
$latex \displaystyle e^{-x} = \sum_{n \geq 0} \frac{(-x)^n}{n!} = \sum_{n \geq 0} (-1)^n \frac{x^n}{n!}.$
</div></p>
<p>But hold on a minute, what does $latex E(-X)$ really mean, formally? It is the composition of the species $latex E$ with the virtual species $latex -X$, and it turns out that it is not at all <em>a priori</em> obvious how to define composition for virtual species! We can find the definition on <a href="https://books.google.com/books?id=83odtWY4eogC&amp;q=127#v=snippet&amp;q=127&amp;f=false">p. 127 of Bergeron <em>et al.</em></a> A special case (which is enough for our present purposes) is</p>
<p><div style="text-align:center;">
$latex \displaystyle \Phi(X - Y) = \Phi(X + Y) \times (E(X)E^{-1}(Y))$
</div></p>
<p>where $latex X$ and $latex Y$ are two sorts of atoms, and $latex \times$ denotes <em>Cartesian</em> product of species. In our case,</p>
<p><div style="text-align:center;">
$latex \displaystyle E(0 - X) = E(0 + X) \times ((E(0) E^{-1}(X)) = E(X) \times E^{-1}(X) = E^{-1}(X)$
</div></p>
<p>since $latex E$ is the identity for Cartesian product (overlaying an additional $latex E$ structure on a set of labels does not add any structure, since there is only one possible $latex E$-structure).</p>
<p>All of this is to say, $latex E(-X)$ is actually <em>defined as</em> $latex E^{-1}$! So at first glance it may seem we actually have nothing to prove: $latex E(-X)$ and $latex E^{-1}$ are the same by definition, end of story. …but in fact, all we have done is shift the burden of proof elsewhere: now it is our intuitive idea of $latex E(-X)$ representing signed sets that requires proof!</p>
<p>To sum up, we know that $latex E(-X) = E^{-1} = \sum_k (-1)^k (E_+)^k$ is the species of signed ballots, with sign given by parity of the number of parts; and intuitively, we also believe that $latex E(-X)$ should correspond to parity-signed sets, $latex \sum_n (-1)^n E_n$. So, is there a nice <em>combinatorial</em> proof showing the correspondence between signed sets and signed ballots?</p>
<p>One can use the law of excluded middle to show that the answer must be “yes”: suppose the answer were “no”; but then I would not be writing this blog post, which is a contradiction since I <em>am</em> writing this blog post. But is there a constructive proof? Fear not! This blog post has gotten long enough, so I will stop here for now and let interested readers puzzle over it; in my next post I will explain what I came up with, along with some musings on linear orders and naturality.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>I am indeed using the word <em>natural</em> in a technical, categorical sense here! This will play an important role in my second post…<a href="#fnref1">↩</a></p></li>
</ol>
</div>

