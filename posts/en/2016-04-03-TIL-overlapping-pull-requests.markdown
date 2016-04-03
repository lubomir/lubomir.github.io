---
title: Today I Learned: Overlapping pull requests in Pagure
tags: TIL, Fedora, Pagure
---

The other day I was wondering what happens when two pull requests in [Pagure]
share some commits. So I tested it out and here are the results.

[Pagure]: https://pagure.io/

By sharing commits I mean one pull request is based on another. (In the spirit
of `git log`, newest commits on the top.)

```
 * d69de04 (pr2) Extra commit
 * 9657821 (pr1) Shared commit
 /
*  4ec8ad5 (master) Last commit on master
```

Assuming both pull requests are to be merged, there are two ways to approach
this.

1. Merge `pr1` first. The merge happens as usual, and `pr2` will get a
   notification comment saying it was rebased. Merging `pr2` now can still use
   fast-forward strategy.

2. Merge `pr2` first. Again, there is no problem merging, and in this situation
   Pagure will say that there are no changes on `pr1` to be merged, so you can
   just close this pull request and be done with it.

Now I don't know if it was designed like this or its just an outcome of the
ways things are implemented, but it seems to behave logically. As far as I know
this is not documented, so I would not be surprised to see changes.

What happens when one pull request is not just a subset of another, but is a
diverging history? In the example, think `pr1` has also an extra commit not
shared with `pr2`.

No matter which request is merged first, the other will be automatically
rebased so that it is only requesting to merge the extra commits. It may or may
not create a merge conflict. This really depends on the nature of the actual
changes.
