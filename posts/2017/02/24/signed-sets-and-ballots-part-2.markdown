---
title: Signed sets and ballots, part 2
published: 2017-02-24T15:16:45Z
categories: combinatorics,math
tags: ballots,bijection,involution,natural,sets,signed,species,virtual
---

<p>Recall, from <a href="https://byorgey.wordpress.com/2017/02/20/signed-sets-and-ballots-part-1/">my previous post</a>, that our goal is to find a <em>combinatorial</em> proof showing the correspondence between signed sets and signed ballots, where a <em>signed set</em> is just a set of $latex n$ elements, considered positive or negative according to the parity of $latex n$, and a <em>signed ballot</em> is an ordered list of sets, considered positive or negative according to the parity of the number of sets.</p>
<p>So, how should such a proof look? For a given number of labels $latex n$, there is a single signed set structure, which is just the set of labels itself (with a sign depending on the parity of $latex n$). On the other hand, there are lots of ballots on $latex n$ labels; the key is that some are positive and some are negative, since the sign of the ballots depends on the parity of the number of parts, not the number of labels. For example, consider $latex n = 3$. There is a single (negative) signed set structure:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/c8dadbdd1a4a1256.png" /></p>
</div>
<p>(I will use a dashed blue line to indicate negative things, and a solid black line for positive things.)</p>
<p>On the other hand, as we saw <a href="https://byorgey.wordpress.com/2017/02/20/signed-sets-and-ballots-part-1/">last time</a>, there are 13 ballot structures on 3 labels, some positive and some negative:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/1c54d7738a0e2be6.png" /></p>
</div>
<p>In this example, it is easy to see that most of the positives and negatives cancel, with exactly one negative ballot left over, which corresponds with the one negative set. As another example, when $latex n = 4$, there is a single positive set, and 75 signed ballots:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/55d603bded1137d5.png" /></p>
</div>
<p>This time it is not quite so easy to tell at a glance (at least not the way I have arranged the ballots in the above picture!), but in fact one can verify that there are exactly 37 negative ballots and 38 positive ones, again cancelling to match the one positive set.</p>
<p>What we need to show, then, is that we can pair up the ballots in such a way that positive ballots are matched with negative ballots, with exactly one ballot of the appropriate sign left to be matched with the one signed set. This is known as a <em>signed involution</em>: an involution is a function which is its own inverse, so it matches things up in pairs; a signed involution sends positive things to negative things and vice versa, except for any fixed points.</p>
<p>In order to do this, we will start by assuming the set of labels is linearly ordered. In one sense this is no big deal, since for any finite set of labels we can always just pick an arbitrary ordering, if there isn’t an “obvious” ordering to use already. On the other hand, it means that the correspondence will be specific to the chosen linear ordering. All other things being equal, we would prefer a correspondence that depends solely on the structure of the ballots, and not on any structure inherent to the labels. I will have quite a bit more to say about this in my third and (probably) final post on the topic. But for today, let’s just see how the correspondence works, given the assumption of a linear order on the labels. I came up with this proof independently while contemplating <a href="http://akc.is/blog/2017-02-18-Inverse-species-and-sign-reversing-involutions.html">Anders Claesson’s post</a>, though it turns out that the exact same proof is already in <a href="http://www.combinatorics.org/ojs/index.php/eljc/article/view/v21i4p16">a paper by Claesson and Hannah</a> (in any case it is really just a small lemma, the sort of thing you might give as a homework problem in an undergraduate course on combinatorics).</p>
<p>Given some ballot, find the <em>smallest</em> label. For example, if the labels are $latex \{1, \dots, n\}$ as in the examples so far, we will find the label $latex 1$.</p>
<ul>
<li><p>If the smallest label is contained in some part together with at least one other label, separate it out into its own part by itself, and put it to the right of its former part. Like this:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/aca212b025804418.png" /></p>
</div></li>
<li><p>On the other hand, if the smallest label is in a part by itself, merge it with the part on the left (if one exists). This is clearly the inverse of the above operation.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/5f0f2b812d6bc1a0.png" /></p>
</div></li>
<li><p>The only case we haven’t handled is when the smallest label is in a part by itself which is the <em>leftmost</em> part in the ballot. In that case, we leave that part alone, switch to considering the <em>second</em>-smallest label, and recursively carry out the involution on the remainder of the ballot.</p>
<p>For example:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/17a4dd855ed2e8c4.png" /></p>
</div>
<p>In this case we find the smallest label (1) in a part by itself in the leftmost position, so we leave it where it is and recurse on the remainder of the ballot. Again, we find the smallest remaining label (2) by itself and leftmost, so we recurse again. This time, we find the smallest remaining label (3) in a part with one other label, so we separate it out and place it to the right.</p></li>
</ul>
<p>This transformation on ballots is clearly reversible. The only ballots it doesn’t change are ballots with each label in its own singleton part, sorted from smallest to biggest, like this:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/c60ed358cf8efa8e.png" /></p>
</div>
<p>In this case the algorithm recurses through the whole ballot and finds each smallest remaining label in the leftmost position, ultimately doing nothing. Notice that a sorted ballot of singletons has the same sign as the signed set on the same labels, namely, $latex (-1)^n$. In any other case, we can see that the algorithm matches positive ballots to negative and vice versa, since it always changes the number of parts by 1, either splitting one part into two or merging two parts into one.</p>
<p>Here’s my implementation of the involution in Haskell:</p>
<pre><code>type Ballot = [[Int]]

ballotInv :: Ballot -&gt; Ballot
ballotInv = go 1
  where
    go _ [] = []
    go s ([a]:ps)
      | s == a = [a] : go (s+1) ps
    go s (p:ps)
      | s `elem` p = delete s p : [s] : ps
    go s (p:[a]:ps)
      | s == a = sort (a:p) : ps
    go s (p:ps) = p : go s ps</code></pre>
<p>(The call to <code>sort</code> is not strictly necessary, but I like to keep each part canonically sorted.)</p>
<p>Here again are the 13 signed ballots for $latex n = 3$, this time arranged so that the pair of ballots in each row correspond to each other under the involution, with the leftover, sorted ballot by itself at the top.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2017/02/99df466df8234d85.png" /></p>
</div>
<p>If you’d like to see an illustration of the correspondence for $latex n = 4$, you can <a href="https://byorgey.files.wordpress.com/2017/02/ballots41.png">find it here</a> (I didn’t want to include inline since it’s somewhat large).</p>
<p>This completes the proof that signed sets and signed ballots correspond. But did we really need that linear order on the labels? Tune in next time to find out!</p>

