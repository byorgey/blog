---
title: First explorations in computer music
published: 2009-03-15T18:04:46Z
categories: haskell,learning,music
tags: ALSA,ChucK,computer music,Haskore,hsc,jack,live coding,SuperCollider
---

This week, I have taken some first steps in exploring computer music and live coding.  Seeing as I am a
<ul>
	<li>classical and jazz pianist,</li>
	<li>amateur composer, and</li>
	<li>programming nerd,</li>
</ul>
it seems odd that it's taken me so long to get around to combining these interests; but it's one of those things that I've intended to explore for a long time and have just never gotten around to until now.  Anyway, I have what I think are some really fantastic ideas about possibilities for real-time musical expression using computer-based tools---but at the moment there is a rather large gap between my ambitions and the reality of my knowledge and abilities.  But that is obviously to be expected.  Hopefully in a few years I'll be posting videos of my awesome live performances; in the meantime I thought I'd write about my experiences getting things set up.  More below the fold.

<!--more-->

The first thing I did was install <a href="http://chuck.cs.princeton.edu/">ChucK</a> (just with apt-get install), which, when run, promptly died with some error about jack.  I didn't know jack about jack (haha), just vaguely that it has something to do with linux audio.  So I opted to instead download the latest stable version of ChucK myself and compile it for ALSA instead (which I again know nothing about, but some poking around seemed to suggest that it was already installed, as opposed to jack).  Compiling ChucK from source involved several false starts where I had to figure out which packages to install based on the header files it was complaining did not exist, but I finally got it to compile cleanly, and this time it ran with no errors on a test file---but no sound.  Some Googling didn't turn up anything useful, but in the process I realized that I still hadn't upgraded to Ubuntu Intrepid, which I wanted to do anyway, so I figured it couldn't hurt, especially if the problem was some ALSA bug related to my particular sound card, or something like that.  So I set it upgrading overnight.  After the upgrade was complete---voila!  ChucK now produced sound!  I guess there was indeed some sort of bug involved.

I currently do intend to come back and learn ChucK more thoroughly at some point, but for now I only fiddled around with it a bit. I was also eager to try out <a href="http://www.slavepianos.org/rd/">Rohan Drape's</a> <a href="http://www.slavepianos.org/rd/f/207949/">Haskell bindings</a> for <a href="http://supercollider.sourceforge.net/">SuperCollider</a>, hsc, since I'd much rather be able to program in <a href="http://www.haskell.org/">Haskell</a> than ChucK per se, and I'm also interested in checking out Henning Thielemann's <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/haskore%2Dsupercollider">haskore-supercollider</a> package, which allows performance of <a href="http://www.haskell.org/haskellwiki/Haskore">Haskore</a> music via hsc.  I imagine that X --&gt; Haskore --&gt; hsc --&gt; SuperCollider could be a very nice toolchain for writing interactive tools X to generate music in real time.  

I got hsc installed with little fuss, thanks to the fantastic <a href="http://hackage.haskell.org/trac/hackage/wiki/CabalInstall">cabal-install tool</a>.  Installing SuperCollider itself was also not too difficult.  Getting everything to work took considerably more effort.  I did end up installing jack on the recommendation of some tutorial or mailing list post or other, along with qjackctl which seems like a nice GUI interface for jack.  I first tried to get SuperCollider working on its own (using its native language, 'sclang', instead of the Haskell bindings), which took a while: after a while I got all the necessary things installed, and I could start up the SC server with no errors, and I could issue commands to it which didn't produce errors, but there was still no sound.  Finally in a stroke of luck I realized that I had to open up the 'connections' dialog in qjackctl and actually connect the SC server's output to the sound card input!  Why this doesn't happen automatically, I have no idea (it happens automatically for ChucK when run using jack).

The next step was getting hsc to produce sound via the emacs mode.  Unfortunately, I was initially led astray by an old mailing list post which was the only documentation I was able to find---it turns out that the sample code shown in the post still type-checks but no longer produces any sound!  So I spent a while frustratedly trying to figure out why everything seemed to be working but I wasn't getting any sound from my hsc commands, even though executing similar sclang commands worked fine!  I e-mailed the haskell-art mailing list, and Rohan Drape kindly and helpfully pointed out the tutorial that comes with hsc, which I had overlooked.  Sure enough, everything worked fine!  So now I have successfully used hsc to get my computer to produce various bleatings and warblings.  (Sum of a sine and triangle wave, being panned left to right according to a different sine wave, anyone? =)

So far, it's been a slow process figuring out how to use hsc.  It comes with a lot of great examples, but some sort of tutorial would be nice; without some sort of overarching guide, the sheer number of examples is overwhelming, since it's hard to know what to look at first.  I tried working through a SuperCollider tutorial while attempting to translate the sclang code into hsc code, with some success.  But the hsc documentation also sometimes leaves a bit to be desired, especially since pretty much everything is a UGen.  For example, here's the documentation for pan2:

<pre>
pan2 :: UGen -&gt; UGen -&gt; UGen -&gt; UGen
Two channel equal power panner. 
</pre>

So, obviously, it does some sort of panning, but there's no indication what the different arguments do, and looking at the source was not helpful at all (it's implemented in terms of some general combinator, which is implemented in terms of yet another more general combinator, and keeping track of which arguments end up getting used where and how that translates into what they do is hopeless unless you're already familiar with the inner workings of hsc and SuperCollider).  I eventually figured it out by trial and error (incidentally, trying to debug a program by <i>listening</i> to its output was a new experience =), but there are lots of other functions like this, with type signatures like <code>Rate -&gt; UGen -&gt; UGen -&gt; UGen -&gt; UGen -&gt; UGen -&gt; Foo</code> and no indication of what the different arguments correspond to.

But this is just a cosmetic complaint, and hopefully can be improved with time (indeed, I very well may contribute some improved documentation myself, once I become more familiar with it) and so far I have no complaints about the library itself, which seems quite well-designed and idiomatically Haskellish.

Anyway, I'll probably write down more thoughts and experiences as I progress, although it remains to be seen how much time I'll have for this once classes start back up (right now I'm on spring break).  I'm hoping that I'll be able to find regular bits of time to devote to it so that I can continue learning and becoming more comfortable with various systems.

