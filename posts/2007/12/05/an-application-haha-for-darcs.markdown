---
title: 'an application (haha) for darcs'
published: 2007-12-05T22:18:49Z
categories: grad school
tags: darcs,essay,personal statement
---

So, I'm currently in the process of applying to grad schools.  Since no grad school application would be complete without the venerable <i>personal statement</i>, I have written a generic personal statement template which makes me sound totally awesome (...because, of course, I am. This whole "selling myself" thing still feels rather unnatural. Oh well.)  The idea is that once I have a nice template, I can proceed to tailor it for each individual school to which I am applying.  Sounds good, right?

But it's not quite that simple.  The template is pretty much all written, but I'm still not quite happy with it. Squeezing the last bits of perfection out of an essay isn't something you can do in fifteen minutes, or even all in one sitting.  And in the meantime I'd really like to get started tailoring it for different schools.  And even if I was perfectly happy with it in its current state, that's no guarantee I won't decide to change something later.  And of course, as soon as I start making tailored copies, any changes I make to the basic essay will have to be merged into the copies.  Ugh.  So I was thinking to myself, "Hmm, there's got to be a nice way to keep multiple, slightly divergent copies of a document and still be able to merge subsequent changes easily...  oh, wait, duh."

That's right, I set up the main template essay in a darcs repository, and then created each of the tailored versions as copies using 'darcs get'.  Now I can tailor the individual versions to my heart's content, while still being able to easily merge in improvements to the basic essay.  So far, it's worked like a charm.  Nothing earth-shattering, of course, and I'm sure I'm not the first person to think of this, but it seemed like a neat, elegant, somewhat non-standard use of darcs, so I thought I would share.

