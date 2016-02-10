---
title: Today I Learned: Scrolling in screen
tags: TIL, screen
---

By default, `screen` does not allow scrolling with regular `PgUp`/`PgDn` or
arrow keys. However, there is a way to move around: the is a *copy mode*.

This mode can be entered by pressing the regular prefix combination (`^A`), and
then hitting `Esc`. Moving around is possible with arrow keys. To get back to
regular mode hit `Esc` twice.

[Source at Unix Stack Exchange](http://unix.stackexchange.com/questions/40242/scroll-inside-screen-or-pause-output).

Even better option is to put following into `.screenrc`:

```
termcapinfo xterm* ti@:te@
```

This will enable regular scrolling with mouse wheel or terminal scrollbar.

[Source at Unix Stack Exchange](http://unix.stackexchange.com/questions/18006/can-mouse-wheel-scrolling-work-in-a-screen-session)
