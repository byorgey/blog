---
title: An interesting monoid
published: 2008-04-17T19:37:24Z
categories: math
tags: knowledge,monoid,preorder
---

The other day I was just sort of letting my mind wander, and I came up with an interesting monoid, which I'm calling the "monoid of partial knowledge". So I thought I'd write about it here, partly just because it's interesting, and partly to see whether anyone has any pointers to any literature (I'm sure I'm not the first to come up with it).

Recall that a <a href="https://secure.wikimedia.org/wikipedia/en/wiki/Monoid"><i>monoid</i></a> is a set with an associative binary operation and a distinguished element which is the identity for the operation.

Now, given a <a href="https://secure.wikimedia.org/wikipedia/en/wiki/Total_order"><i>total order</i></a> on a set $latex S$ with a smallest element $latex e$, we get a monoid $latex (S, \max)$, where $latex \max$ denotes the function which determines the larger of two elements, according to the total order on $latex S$.  $latex \max$ is clearly associative, and has identity $latex e$.  Taking a list of elements of $latex S$ and summarizing it via this monoid corresponds to finding the maximum element in the list.  If you think of receiving the elements of the list one by one, and applying $latex \max$ to each new incoming value and the value of an accumulator (storing the result back into the accumulator, which should obviously be initialized to $latex e$), at any given time the value of the accumulator represents the 'current best', i.e. the largest element among those received so far.

The idea I had was to generalize this from a total order to a preorder.  Recall that a <a href="https://secure.wikimedia.org/wikipedia/en/wiki/Preorder"><i>preorder</i></a> is a set $latex S$ equipped with a reflexive, transitive binary relation, often denoted $latex \lesssim$.  That is, for any $latex x,y,z \in S$, we have $latex x \lesssim x$; and $latex x \lesssim y \land y \lesssim z$ implies $latex x \lesssim z$. If $latex \lesssim$ is also antisymmetric, that is, $latex x \lesssim y \land y \lesssim x$ implies $latex x = y$, it is called a <i>partial order</i>, or <i>poset</i>.  Then if $latex x \lesssim y$ or $latex y \lesssim x$ for any two elements $latex x$ and $latex y$, we get a total order, but for a general preorder some pairs of elements may not be comparable -- that is, there may be elements $latex x$ and $latex y$ for which neither $latex x \lesssim y$ nor $latex y \lesssim x$ holds.

Let's think about this.  Suppose we are given a preorder $latex (P,\lesssim)$ with an <i>initial object</i> $latex e$ (an initial object in this context is an element which is $latex \lesssim$ all other elements).  We'll initialize an accumulator to $latex e$, and imagine receiving elements of $latex P$ one at a time.  For a concrete example, suppose we are dealing with the preorder (actually also a poset) of positive integers under the divisibility relation, so our accumulator is initialized to 1.  Let's say we receive the integer 4.  Clearly, 1 divides 4, so we should replace the 1 in our accumulator with 4.  But now suppose we next receive the integer 5.  4 does not divide 5 or vice versa, so what should we do?  We would be justified in neither throwing the 5 away nor replacing the 4, since 4 and 5 are not related to each other under the divisibility relation.  Somehow we need to keep the 4 <i>and</i> the 5 around.

The solution is that instead of creating a monoid over $latex P$ itself, as we can for sets with a total order, we create a monoid over <i>subsets</i> of $latex P$.  In particular, consider the set $latex P_*$ of subsets of $latex P$ which do not contain two distinct elements $latex x,y$ for which $latex x \lesssim y$.  Since we are dealing with subsets of $latex P$, we can actually drop the restriction that $latex P$ contain an initial object; the empty set will serve as the identity for the monoid.

We then define the monoid operation $latex \oplus$ on two such subsets as

$latex S \oplus T = (S \triangleleft T) \cup (S \triangleright T)$

where

$latex S \triangleleft T = \{ s \in S \mid \forall t \in T, s = t \mbox{ or } t \lesssim s \mbox{ or } s \not \lesssim t \}$

and

$latex S \triangleright T = \{ t \in T \mid \forall s \in S, s = t \mbox{ or } t \not \lesssim s \}$.

In words, we combine subsets $latex S$ and $latex T$ by forming the set of objects from $latex S \cup T$ which are not $latex \lesssim$ any others, with the exception of objects $latex s \in S, t \in T$ where both $latex s \lesssim t$ and $latex t \lesssim s$; in this case we keep $latex s$ but not $latex t$. This introduces a "left bias" to $latex \oplus$; there is also an equally valid version with right bias (in particular, $latex S \oplus' T = (T \triangleleft S) \cup (T \triangleright S)$).

Now, let's show that this really does define a valid monoid.  First, we need to show that $latex \oplus$ is closed over $latex P_*$. Suppose $latex S, T \in P_*$.  Suppose also that $latex x,y \in S \oplus T$ are distinct elements of $latex P$ with $latex x \lesssim y$; we'll derive a contradiction.  First, we cannot have $latex x,y \in S$ or $latex x,y \in T$ by definition of $latex P_*$.  Now suppose $latex x \in T, y \in S$.  The fact that $latex x \in S \oplus T$ together with the definition of $latex S \triangleright T$ imply that we must have $latex x \not \lesssim y$, a contradiction.  Finally, suppose $latex x \in S, y \in T$.  Again, by the definition of $latex S \triangleright T$ we must have $latex y \not \lesssim x$.  But then the fact that $latex x \in S \oplus T$, together with the definition of $latex S \triangleleft T$ and the facts that $latex x \neq y$ and $latex y \not \lesssim x$ imply that $latex x \not \lesssim y$, a contradiction again.  Hence $latex S \oplus T$ contains no such pair of elements.

The fact that the empty set $latex \varnothing$ is the identity for $latex \oplus$ is clear. (Incidentally, this is why we require that none of the sets in $latex P_*$ contain two distinct elements with one $latex \lesssim$ the other: if $latex S$ were such a set, we would have $latex \varnothing \oplus S \neq S$.)  I leave the associativity of $latex \oplus$ as an exercise for the reader (translation: this post is already getting long, the associativity of $latex \oplus$ seems intuitively obvious to me, and I don't feel like formalizing it at the moment -- perhaps I'll try writing it up later).  I also leave as an interesting exercise the following theorem: if $latex S, T \in P_*$ are both finite and nonempty, then $latex S \oplus T$ is also finite and nonempty.

In our example from before, we could now begin with $latex \{1\}$ in our accumulator.  After receiving the singleton set $latex \{4\}$, our accumulator would have that singleton set as its new value.  Upon receiving $latex \{5\}$, our accumulator would become $latex \{4,5\}$.  Receiving $latex \{10\}$ would result in $latex \{4,10\}$ (5 divides 10, so the 5 is discarded); if we later received $latex \{20\}$, we would simply have $latex \{20\}$ in our accumulator (both 4 and 10 divide 20).

I like to think of this as the monoid of <i>partial knowledge</i>.  If we consider $latex P$ to be a set of facts or beliefs, some better (more reliable, useful, correct, complete, etc.) than others, then elements of $latex P_*$ correspond to possible sets of beliefs.  $latex \oplus$ describes how a set of beliefs changes upon encountering a new set of facts; some of the new facts may supersede and replace old ones, some may not impart any new information, and some may be completely new facts that aren't related to any currently known.

Now, why can this be thought of as a generalization of the monoid $latex (P, \max)$ on a totally ordered set?  Well, look what happens when we replace $latex P$ in the definitions above with a totally ordered set with relation $latex \leq$: first of all, the restriction on $latex P_*$ (no two elements of a set in $latex P_*$ should be related by $latex \leq$) means that $latex P_*$ contains only the empty set and singleton sets, so (ignoring the empty set) $latex P_*$ is isomorphic to $latex P$.  Now look at the definition of $latex S \triangleleft T$, with $latex \lesssim$ replaced by $latex \leq$ (and $latex \not \lesssim$ replaced by $latex &gt;$):

$latex S \triangleleft T = \{ s \in S \mid \forall t \in T, s = t \mbox{ or } t \leq s \mbox{ or } s &gt; t \}$

But $latex s = t$ and $latex s &gt; t$ are both subsumed by $latex t \leq s$, so we can rewrite this as

$latex \{s\} \triangleleft \{t\} = \{s\} \mbox{ if } s \geq t, \mbox{ or } \varnothing \mbox{ otherwise }$.

An analysis of $latex \triangleright$ is similar, and it is clear that $latex \{s\} \oplus \{t\} = \{\max(s,t)\}$.

I note in passing that although it might appear shady that I swept that "ignoring the empty set" bit under the rug, everything really does check out: technically, to see a direct generalization of $latex (P,\max)$ to $latex (P_*, \oplus)$, we can require that $latex P$ have an initial object and that $latex P_*$ contains only finite, nonempty sets.  Then it requires a bit more work to prove that $latex \oplus$ is closed, but it still goes through.  I used the formulation I did since it seems more general and requires less proving.

Anyway, this ended up being longer than I originally anticipated (why does that always happen!? =), so I'll stop here for now, but next time I'll give some actual Haskell code (which I think ends up being pretty neat!), and talk about one relatively common design pattern which is actually a special case of this monoid.


