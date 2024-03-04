---
katex: true
title: 'Patch theory thoughts, part I'
published: 2008-02-07T20:19:22Z
categories: haskell,learning,projects
tags: collaborative editing,darcs,patch theory
---

So, I still don't know whether I'll actually end up writing a <a href="http://byorgey.wordpress.com/2008/02/04/gobby-haskell-and-patch-theory/">gobby clone in Haskell</a>.  But it's already been a wild ride thinking about the theory behind it and some of the issues involved, and over the next few posts I'd like to share some of my thoughts, complete with illustrative code, in the hopes that others will find them interesting or inspiring. Will this be just a rehashing of stuff that someone else has already written about in much more depth?  Will it be an important contribution to the topic?  Frankly, at this stage, I don't really care. =)

I'll get to the fun stuff soon; today, I'd like to start out simple by just responding to a few of the comments on my previous post.

First, Creighton Hogg wondered about using <a href="http://darcs.net/">darcs</a> as the backend of such a concurrent editor, and whether it would run into issues with the known performance problems with darcs.  Allow me to first clarify that I am not thinking about creating a concurrent editor backed by darcs, but rather one which is backed by the <i>theory behind</i> darcs.  It's still a fair question, though: mightn't my editor exhibit the same algorithmic problems that causes darcs performance problems?  The answer, as it turns out, is no: darcs exhibits performance problems in the specific case of <a href="http://wiki.darcs.net/DarcsWiki/ConflictsFAQ#head-882e1bd93393425e47d1699ed72f9c4053f75ef2">merging conflicting patches</a>, but in a concurrent editor (the model I'm interested in, at least) there's no such thing as conflicting patches!  This is a very important difference and I'll talk about this, and its theoretical implications, later.  In any event, darcs 2 reportedly fixes the performance problems anyway.

Tim suggested some sort of GUI interface for darcs -- I think this is a great idea, but in my opinion doesn't come anywhere close to filling the same niche as a collaborative editor.  A collaborative editor allows for much more freedom, and much more...well... collaboration.  For example, you could change some code and put a little comment next to it saying "is this right? Or should it be like XYZ?", and someone else could respond to the comment, or change the code, and so on.  No one would do that via darcs; the social assumption is that each darcs patch you make is correct (to the best of your knowledge), so there's not as much room for experimenting and getting feedback from others.  Anon also questioned the worth of this sort of project, claiming that collaborative editors don't actually get used all that much, to which I would reply: maybe the reason people don't use collaborative editors very much is because good (and free) ones don't exist.

sclv, Eric Kow, and the Edward linked to some <a href="http://en.wikipedia.org/wiki/Operational_transformation">interesting</a> <a href="http://cs-people.bu.edu/dgd/thesis/original_paper.html">research</a> on the topic, which I've started reading a bit but hope to look into in more detail soon.  

mgsloan is working on a <a href="http://mgsloan.nfshost.com/darcs/haskroom/">cool Haskell editor project</a> (<a href="http://mgsloan.nfshost.com/haskroom.png">screenshot</a>) -- although as Christophe Poucet points out, it would not work well with a collaborative model due to its use of zippers to hold buffer contents.  But I'm glad there are other people out there who know how to make cool-looking GUIs, since I sure don't. =)

Finally, it turns out that I'm not the first person to have this idea!  Christophe (along with Ivan Tarasov) worked on the beginnings of a system called <a href="http://code.haskell.org/splatch/">Splatch</a> at last October's Haskell Hackathon.  It's pretty neat, although (as I have expressed to Christophe, rather incoherently, I'm afraid) I believe in its current state it has a few fundamental theoretical problems.  Over my next few posts I hope to be able to expound on this a bit more, although my focus will not be on evaluating Splatch per se, but on describing my explorations in patch theory in general, and as it might apply to a collaborative editor in particular.

