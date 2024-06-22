---
katex: true
title: 'BlogLiterately 0.5.2 release, with improved image uploading'
published: 2012-08-27T01:43:37Z
categories: haskell,writing
tags: image,release,upload
---

<p>Just a quick note to say that I've just released <a href="http://hackage.haskell.org/package/BlogLiterately-0.5.2">version 0.5.2 of BlogLiterately</a>. (For more information about what it does, see what I've written about previous releases <a href="https://byorgey.github.io/blog/posts/2012/07/02/blogliterately-0-4-release.html">here</a> and <a href="https://byorgey.github.io/blog/posts/2012/07/07/blogliterately-0-5-release.html">here</a>, and the <a href="http://byorgey.wordpress.com/blogliterately/">documentation</a>.) The new version keeps track of which image files have been uploaded---and their corresponding URLs on the server---in a special dotfile, so it only uploads a given image once, even across multiple runs of <code>BlogLiterately</code>. This is quite convenient, since it means that one can simply leave the <code>--upload-images</code> flag on when uploading successive drafts of a post, and images will be uploaded only once and available to be viewed in the preview version of your post.</p>

