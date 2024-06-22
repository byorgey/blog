---
katex: true
title: 'An interesting monoid'
published: 2008-04-17T19:37:24Z
categories: math
tags: knowledge,monoid,preorder
---

The other day I was just sort of letting my mind wander, and I came up with an interesting monoid, which I'm calling the "monoid of partial knowledge". So I thought I'd write about it here, partly just because it's interesting, and partly to see whether anyone has any pointers to any literature (I'm sure I'm not the first to come up with it).

Recall that a <a href="https://secure.wikimedia.org/wikipedia/en/wiki/Monoid"><i>monoid</i></a> is a set with an associative binary operation and a distinguished element which is the identity for the operation.

Now, given a <a href="https://secure.wikimedia.org/wikipedia/en/wiki/Total_order"><i>total order</i></a> on a set $S$ with a smallest element $e$, we get a monoid $(S, \max)$, where $\max$ denotes the function which determines the larger of two elements, according to the total order on $S$.  $\max$ is clearly associative, and has identity $e$.  Taking a list of elements of $S$ and summarizing it via this monoid corresponds to finding the maximum element in the list.  If you think of receiving the elements of the list one by one, and applying $\max$ to each new incoming value and the value of an accumulator (storing the result back into the accumulator, which should obviously be initialized to $e$), at any given time the value of the accumulator represents the 'current best', i.e. the largest element among those received so far.

The idea I had was to generalize this from a total order to a preorder.  Recall that a <a href="https://secure.wikimedia.org/wikipedia/en/wiki/Preorder"><i>preorder</i></a> is a set $S$ equipped with a reflexive, transitive binary relation, often denoted $\lesssim$.  That is, for any $x,y,z \in S$, we have $x \lesssim x$; and $x \lesssim y \land y \lesssim z$ implies $x \lesssim z$. If $\lesssim$ is also antisymmetric, that is, $x \lesssim y \land y \lesssim x$ implies $x = y$, it is called a <i>partial order</i>, or <i>poset</i>.  Then if $x \lesssim y$ or $y \lesssim x$ for any two elements $x$ and $y$, we get a total order, but for a general preorder some pairs of elements may not be comparable -- that is, there may be elements $x$ and $y$ for which neither $x \lesssim y$ nor $y \lesssim x$ holds.

Let's think about this.  Suppose we are given a preorder $(P,\lesssim)$ with an <i>initial object</i> $e$ (an initial object in this context is an element which is $\lesssim$ all other elements).  We'll initialize an accumulator to $e$, and imagine receiving elements of $P$ one at a time.  For a concrete example, suppose we are dealing with the preorder (actually also a poset) of positive integers under the divisibility relation, so our accumulator is initialized to 1.  Let's say we receive the integer 4.  Clearly, 1 divides 4, so we should replace the 1 in our accumulator with 4.  But now suppose we next receive the integer 5.  4 does not divide 5 or vice versa, so what should we do?  We would be justified in neither throwing the 5 away nor replacing the 4, since 4 and 5 are not related to each other under the divisibility relation.  Somehow we need to keep the 4 <i>and</i> the 5 around.

The solution is that instead of creating a monoid over $P$ itself, as we can for sets with a total order, we create a monoid over <i>subsets</i> of $P$.  In particular, consider the set $P_*$ of subsets of $P$ which do not contain two distinct elements $x,y$ for which $x \lesssim y$.  Since we are dealing with subsets of $P$, we can actually drop the restriction that $P$ contain an initial object; the empty set will serve as the identity for the monoid.

We then define the monoid operation $\oplus$ on two such subsets as

$S \oplus T = (S \triangleleft T) \cup (S \triangleright T)$

where

$S \triangleleft T = \{ s \in S \mid \forall t \in T, s = t \mbox{ or } t \lesssim s \mbox{ or } s \not \lesssim t \}$

and

$S \triangleright T = \{ t \in T \mid \forall s \in S, s = t \mbox{ or } t \not \lesssim s \}$.

In words, we combine subsets $S$ and $T$ by forming the set of objects from $S \cup T$ which are not $\lesssim$ any others, with the exception of objects $s \in S, t \in T$ where both $s \lesssim t$ and $t \lesssim s$; in this case we keep $s$ but not $t$. This introduces a "left bias" to $\oplus$; there is also an equally valid version with right bias (in particular, $S \oplus' T = (T \triangleleft S) \cup (T \triangleright S)$).

Now, let's show that this really does define a valid monoid.  First, we need to show that $\oplus$ is closed over $P_*$. Suppose $S, T \in P_*$.  Suppose also that $x,y \in S \oplus T$ are distinct elements of $P$ with $x \lesssim y$; we'll derive a contradiction.  First, we cannot have $x,y \in S$ or $x,y \in T$ by definition of $P_*$.  Now suppose $x \in T, y \in S$.  The fact that $x \in S \oplus T$ together with the definition of $S \triangleright T$ imply that we must have $x \not \lesssim y$, a contradiction.  Finally, suppose $x \in S, y \in T$.  Again, by the definition of $S \triangleright T$ we must have $y \not \lesssim x$.  But then the fact that $x \in S \oplus T$, together with the definition of $S \triangleleft T$ and the facts that $x \neq y$ and $y \not \lesssim x$ imply that $x \not \lesssim y$, a contradiction again.  Hence $S \oplus T$ contains no such pair of elements.

The fact that the empty set $\varnothing$ is the identity for $\oplus$ is clear. (Incidentally, this is why we require that none of the sets in $P_*$ contain two distinct elements with one $\lesssim$ the other: if $S$ were such a set, we would have $\varnothing \oplus S \neq S$.)  I leave the associativity of $\oplus$ as an exercise for the reader (translation: this post is already getting long, the associativity of $\oplus$ seems intuitively obvious to me, and I don't feel like formalizing it at the moment -- perhaps I'll try writing it up later).  I also leave as an interesting exercise the following theorem: if $S, T \in P_*$ are both finite and nonempty, then $S \oplus T$ is also finite and nonempty.

In our example from before, we could now begin with $\{1\}$ in our accumulator.  After receiving the singleton set $\{4\}$, our accumulator would have that singleton set as its new value.  Upon receiving $\{5\}$, our accumulator would become $\{4,5\}$.  Receiving $\{10\}$ would result in $\{4,10\}$ (5 divides 10, so the 5 is discarded); if we later received $\{20\}$, we would simply have $\{20\}$ in our accumulator (both 4 and 10 divide 20).

I like to think of this as the monoid of <i>partial knowledge</i>.  If we consider $P$ to be a set of facts or beliefs, some better (more reliable, useful, correct, complete, etc.) than others, then elements of $P_*$ correspond to possible sets of beliefs.  $\oplus$ describes how a set of beliefs changes upon encountering a new set of facts; some of the new facts may supersede and replace old ones, some may not impart any new information, and some may be completely new facts that aren't related to any currently known.

Now, why can this be thought of as a generalization of the monoid $(P, \max)$ on a totally ordered set?  Well, look what happens when we replace $P$ in the definitions above with a totally ordered set with relation $\leq$: first of all, the restriction on $P_*$ (no two elements of a set in $P_*$ should be related by $\leq$) means that $P_*$ contains only the empty set and singleton sets, so (ignoring the empty set) $P_*$ is isomorphic to $P$.  Now look at the definition of $S \triangleleft T$, with $\lesssim$ replaced by $\leq$ (and $\not \lesssim$ replaced by $>$):

$S \triangleleft T = \{ s \in S \mid \forall t \in T, s = t \mbox{ or } t \leq s \mbox{ or } s > t \}$

But $s = t$ and $s > t$ are both subsumed by $t \leq s$, so we can rewrite this as

$\{s\} \triangleleft \{t\} = \{s\} \mbox{ if } s \geq t, \mbox{ or } \varnothing \mbox{ otherwise }$.

An analysis of $\triangleright$ is similar, and it is clear that $\{s\} \oplus \{t\} = \{\max(s,t)\}$.

I note in passing that although it might appear shady that I swept that "ignoring the empty set" bit under the rug, everything really does check out: technically, to see a direct generalization of $(P,\max)$ to $(P_*, \oplus)$, we can require that $P$ have an initial object and that $P_*$ contains only finite, nonempty sets.  Then it requires a bit more work to prove that $\oplus$ is closed, but it still goes through.  I used the formulation I did since it seems more general and requires less proving.

Anyway, this ended up being longer than I originally anticipated (why does that always happen!? =), so I'll stop here for now, but next time I'll give some actual Haskell code (which I think ends up being pretty neat!), and talk about one relatively common design pattern which is actually a special case of this monoid.


