---
title: Today I Learned: Searching in Vim
tags: Fedora, vim, TIL
---

Ok, I lied. Searching in Vim is something I do all the time, so it's not such a
new thing. However there is a feature that I only need every once in a while,
and always forget how to do it.

The goal is to highlight something in the code, you can just search for it and
if `hlsearch` is on, it will shine with the light of thousand suns.

But what if you need some context to match the snippet, but only want to
highlight part of the match?

Search no more. The magic is done by including `\zs` or `\ze` in the search
pattern. These snippets do not affect what is matched, but they set the start
and end of the match respectively.
