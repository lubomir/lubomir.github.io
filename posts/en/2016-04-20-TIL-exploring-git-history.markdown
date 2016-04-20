---
title: Today I Learned: Exploring Git history
tags: TIL, git, Fedora
---

For a long time my most favourite way of looking at Git history has been this
long command.

```bash
$ git log --graph --oneline --decorate --all
```

All this while it worked very well, but the one annoyance I noticed is that
`--all` actually displays everything. Admittedly this is expected, but in some
cases not really desirable.

In situations when there are multiple remotes configured and one of these
remotes has a long-running branch I don't really care about the command would
not be helpful at all.

Turns out if I read the man page carefully, I could have avoided the issue by
simply using `--branches` instead of `--all`. That will only display local
branches in the graph.

The commits that get displayed will still have annotations about remote
branches that end at that commit, so the context is still there.
