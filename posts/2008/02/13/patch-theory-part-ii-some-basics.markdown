---
katex: true
title: 'Patch theory, part II: some basics'
published: 2008-02-13T19:14:36Z
categories: haskell,learning,projects
tags: category theory,collaborative editing,groupoid,merge,patch theory
---

(Previous posts <a href="http://byorgey.wordpress.com/2008/02/04/gobby-haskell-and-patch-theory/">here</a>, and <a href="http://byorgey.wordpress.com/2008/02/07/patch-theory-thoughts-part-i/">here</a>.)

So, let's talk about patch theory!  I should start by saying that I have obviously drawn a lot of ideas and inspiration from <a href="http://darcs.net/">darcs</a>, and especially from the <a href="http://en.wikibooks.org/wiki/Understanding_darcs">wikibook explanation</a> of darcs patch theory, but I do think there might be some nuggets of original contributions here (in subsequent posts at least), but I'm not yet familiar enough with the literature to really say.

In this post I'd like to start off by giving an overview of the basics of patch theory, to lay a foundation for the things I plan to talk about in future posts.

<h3>Patches and documents</h3>

A <i>patch</i> is essentially a function which takes a <i>document</i> as input and produces another document.  A "document" is just any sort of thing that we might wish to modify.  In the context of darcs, a "document" is an entire directory tree; in a collaborative editor it would be just a single file.  

Note, however, that patches are <i>partial</i> functions: not every patch can be applied to every document!  For example, a patch which (to use a darcs example) makes a modification to file X cannot be applied in a context in which there is no file X.  As another example, a patch to delete the character 'z' from the first position in a file cannot be applied to a file which begins with the character 'y'.  But thinking of patches as partial functions is not a very useful point of view.  The main point is that the <i>context</i> of a patch matters---both the context to which it is applied, and the context which it produces.  We will write $P : x \to y$ to denote a patch $P$ which, when applied to document $x$, produces document $y$. We say that $x$ is the domain of $P$, and $y$ is its codomain.

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/patch.png' alt='A patch' />
</div>

Of course, this immediately suggests...

<h3>The category of patches</h3>

Patches can be most usefully viewed as morphisms in a category with documents as objects.  (If you don't know any <a href="http://en.wikibooks.org/wiki/Haskell/Category_theory">category theory</a>, don't worry: the rest of this post doesn't particularly depend on any background knowledge.) To wit: given two patches $P : x \to y$ and $Q : y \to z$, we can compose them to form the composite patch $PQ : x \to z$, which has the same overall effect as applying first $P$, then $Q$.  (Of course, there are good arguments for writing this composition in the other order, like $QP$---function composition and all that---but this is the way I've been writing it, so get used to it. =) 

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/compose.png' alt='Patch composition' />
</div>

Since patches can be viewed as functions from one document to another, and function composition is associative, patch composition is obviously associative as well.  Finally, for every document $d$, we will have a null patch $id_d$ which sends $d$ to itself.  

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/identity.png' alt='The identity patch for document d' />
</div>


Since we're very interested in the "undo" operation, we also require that every patch must be invertible---that is, for every patch $P : x \to y$ there must be a corresponding patch $P^{-1} : y \to x$, such that $P P^{-1} = id_x$ and $P^{-1} P = id_y$.  Of course, this also means that $(P^{-1})^{-1} = P$.

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/inverse.png' alt='The inverse of a patch' />
</div>

So, the category of patches is actually a <i><a href="https://secure.wikimedia.org/wikipedia/en/wiki/Groupoid">groupoid</a></i>.  A groupoid can be viewed as a set with inverses and a <i>partial</i> binary operation---here, the partiality comes from the fact that not all patches can be composed---but I prefer the category-theoretical view of a groupoid as a category with all morphisms invertible.  Because really, who likes partial functions?

(A quick note: I'm playing a little fast and loose with patch equality here; when I say that two patches are equal, what I <i>really</i> mean is that they are observationally equivalent.  So technically, the morphisms in the category of patches are <i>equivalence classes</i> of patches; in a particular implementation there may be patches with distinguishable representations which nevertheless have the same effect -- that is, they produce the same output document given the same input document.)

<h3>Commutation</h3>

The other central operation on patches is that of <i>commutation</i>.  As a motivating example, let's consider the problem of undo in a text editor.  Suppose, starting from a blank document, you sequentially apply the five patches $A$ through $E$.  Therefore, the current document state can be described by the composite patch

<div align="center">
$ABCDE$
</div>

Now suppose you want to undo your last change.  This is easy: since every patch has an inverse, you can just apply $E^{-1}$ to obtain

<div align="center">
$ABCDEE^{-1} = ABCD$
</div>

(In practice, to remember the fact that you performed an undo, and to allow the possibility of redo in the future, an editor would retain the patches $E$ and $E^{-1}$ rather than deleting them.  But the overall effect is the same.)

Nice.  But what if you are using a collaborative editor, and changes $D$ and $E$ were made by a different user (perhaps you are not even aware of their changes, if they were made in a different part of the document)?  You want to undo patch $C$, which is the last change that you made, but simply applying $C^{-1}$ doesn't work anymore, since $C^{-1}$ cannot be composed with $E$ (the codomain of $E$ does not match the domain of $C^{-1}$)!

We need a way to "move" $C$ to the end of the patch sequence, like this:

<div align="center">
$ABCDE \Rightarrow ABD'C'E \Rightarrow ABD'E'C''$
</div>

Now we can simply apply the inverse patch $C''^{-1}$ to undo.

This process of reordering patches is referred to as <i>commutation</i>.  The composite patch $PQ$ commutes to the composite patch $Q'P'$ (written $PQ \leftrightarrow Q'P'$) if $PQ = Q'P'$, and $P$ represents "the same change" as $P'$, and similarly for $Q$ and $Q'$.  

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/commute.png' alt='Commuting patches' />
</div>

Of course, "the same change" is quite vague, but it's a necessary restriction; just requiring that $PQ = Q'P'$ is not enough, since in that case we could, for example, choose $Q'=P$ and $P'=Q$---obviously not what we want.  I've wondered whether there is a formal way to pin this down, although I think it might depend on the particular document type being used.  However, for now a simple example should suffice.

Suppose Alice and Bob are editing a document together, which contains the word "cap".  First, Alice inserts the letter "m" at position 2 (positions are zero-indexed), to produce the word "camp"; call this patch $A$.  Next, Bob inserts the letter "r" at position 1, producing the word "cramp"; call this patch $B$.  Now Alice decides that she wishes to undo her change, since "crap" is a much better word than "cramp".  In order to do this, the patches $A$ and $B$ must first be commuted: we want to find patches $B'$ and $A'$ such that $B'$ adds the letter "r", $A'$ adds the letter "m", and when composed, $B'A'$ still sends the document "cap" to the document "cramp".  In this case, it's not too hard to see that $B'$ should still insert "r" at position 1, but now $A'$ should insert "m" at position 3 instead of position 2, since the location in the document where $A$ inserted an "m" has been "shifted over" by the patch $B'$.

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/alice-bob-commute.png' alt='Alice and Bob commute their patches.' />
</div>

After commuting $A$ and $B$, the patch $A'^{-1}$ can now be applied to undo Alice's change.

Now, the big question: does every pair of patches commute?  In the case of a version control system like darcs, the answer is definitely "no".  For example, suppose $P$ creates file X, and $Q$ adds some content to file X.  There is no way we can meaningfully reorder the two patches---content cannot be added to file X before it has been created!  In the case of a collaborative editor, on the other hand, the answer is... maybe?  This is one of the central questions I plan to address in later posts.

If the patches $P$ and $Q$ commute, a nice property that we'd like to have hold in all cases is

<div align="center">
$PQ \leftrightarrow Q'P' \leftrightarrow PQ$,
</div>

that is, we want the commute operation to be an involution, so applying it twice is the identity.  This also has some interesting implications for the theory of a collaborative editor, and it will come up again in later posts, too!

<h3>Merging</h3>

The other fundamental operation on patches, of course, is <i>merging</i>: taking two patches made to the same document in parallel, and merging them into a sequence of patches that performs both changes.  In a version control system, this happens all the time, when two people have the same source and start making changes to it at the same time, and later want to get the other's changes as well.  It happens in a collaborative editor, too, because of network latency. When you start typing something, someone else may have already typed some things that have not yet propagated to you over the network; when you finally receive the propagated changes, they will have to be merged with the changes you have made.

It turns out, however, that merging is really not a fundamental operation at all!  It can be implemented very simply in terms of commutation and inverses.  Here's the situation: suppose we have two patches, $A$ and $B$, made in parallel to the same document, $x$.  We want to find a patch $B'$ which performs the "same change" as $B$, but which can be composed with $A$.

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/merge1.png' alt='Merging two patches' />
</div>

How can we find $B'$?  The key is to note that if we invert $A$, this looks just like the diagram for commuting two patches, but on its side!

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/merge2.png' alt='Implementing merge' />
</div>

In other words, to merge patches $A$ and $B$, we first commute $A^{-1}B$ to obtain $B'(A^{-1})'$.  Then we can simply discard $(A^{-1})'$.  (Of course, this sounds like wasted computation, but supposing we were to use some sort of lazy language... =)  Let's illustrate this with a simple example.  Alice and Bob are at it again; this time, they are editing a document containing the word "hat".  Alice adds the letter "c" to create the word "chat" (patch $A$).  At the same time, Bob adds the letter "s" to create the word "hats" (patch $B$).  Now Alice's editor gets Bob's patch, and needs to merge it with Alice's.  Commuting $A^{-1}B$ yields $B' (A^{-1})'$, as shown in the diagram, and Alice's editor applies patch $B'$, producing the document "chats".

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/alice-bob-merge.png' alt='Alice merges Bob’s change' />
</div>

Of course, at the same time, Bob's editor receives Alice's patch, and performs the dual merge shown below.

<div align="center">
<img src='http://byorgey.files.wordpress.com/2008/02/bob-alice-merge.png' alt='Bob merges Alice’s change' />
</div>

This illustrates an obvious and important property that must hold: merging must be symmetric!  From the above two diagrams, we must have $AB' = BA'$.  

<h3>Onward</h3>

Alright, enough for today!  Next time: some actual Haskell code implementing all of this for a simple document type!

