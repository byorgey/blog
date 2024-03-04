---
katex: true
title: 'BlogLiterately 0.8.1, now with HTTPS!'
published: 2015-06-02T02:49:35Z
categories: haskell
tags: BlogLiterately,haxr,HTTPS,Wordpress,XML-RPC
---

<p>I’ve just released <a href="http://hackage.haskell.org/package/BlogLiterately-0.8.1">version 0.8.1 of BlogLiterately</a>, a tool for <a href="http://byorgey.wordpress.com/blogliterately/">formatting and posting stuff (especially Haskelly stuff) to blogs</a>. This is in conjunction with the release of <a href="http://hackage.haskell.org/package/haxr-3000.11">haxr-3000.11</a>. After much blood, sweat, and tears, I was able to rip the <code>HTTP</code> package out of the guts of <code>haxr</code>, replace it with <code>http-streams</code>, and carefully sew everything back together around the edges. The result is that <code>haxr</code> now finally supports making XML-RPC calls via HTTPS, which in turn means that <code>BlogLiterately</code> once again works with Wordpress, which <a href="https://mypersonalblog1984.wordpress.com/2015/01/11/broken-xmlrpc-on-wordpress-com/">no longer supports XML-RPC over HTTP</a>. Happy blogging!</p>
<div class="references">

</div>

