---
title: Today I Learned: Customizing less
tags: TIL, less
---

I have been using `less` wrong. For a long time I have had `LESS=-R` in my
`.bashrc` to make `less` display colors.

Ever since I set this, I've had trouble with `git` paging its output even when
it would fit on a single page. This has bothered me, but I did not connect it
to the `LESS` setting.

It turns out that `git` is quite sophisticated. When its output is paged via
`less`, it looks if `LESS` env var is set, and sets it to `-FRX` if it is not.

The `-F` part is the important one. If tells `less` to not do anything if the
output fits on screen.
