---
title: Today I Learned: Packaging bash-completion files
tags: TIL, Fedora, bash
---

Today I ran into this kind of problem for the first time: a package I needed to
build installed a bash completion file, and the destination for it was
determined by `pkg-config`. Depending on the system, it would most likely end
up in `/usr/share/bash-completion/completions/` or `/etc/bash_completion.d/`.

How to write the spec file to handle this dichotomy? Turns out it is not that
complicated.

Drop this at the top of spec file and then use `%{compdir}` in the `%files`
section. Solved.

```
%define compdir %(pkg-config --variable=completionsdir bash-completion)
%if "%{compdir}" == ""
%define compdir "/etc/bash_completion.d"
%endif
```

The real gotcha is that the `%files` section must install the parent of this
directory, otherwise the builds will fail with error about two files on one
line.
