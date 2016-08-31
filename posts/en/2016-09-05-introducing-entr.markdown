---
title: Introducing entr
tags: Fedora, entr, automation
---

When writing code, my workflow is usually to make edits and re-run the test
suite (that is, if there is one). This is actually a lot of work that I don't
want to do.

Thankfully, it's quite easy to automate. All we need is a utility that would
watch files for change and then run the tests.

On Linux, we can somewhat get by with *inotifywait*, but it is rather tedious.
There must be a better way; and there actually is.

Well, the future is here and its name is [entr]. It runs arbitrary commands
when files change. The list of files to monitor is provided on standard input.

It turned out that I most often want to watch all relevant files in current
directory. Therefore, I created a small wrapper that will find all interesting
files in current directory and pass them to `entr`. I call it `guard`.

```bash
#!/bin/sh
set -o pipefail
ag -g '' | entr "$@"
```

Using `git ls-files` would work almost as well as [the silver searcher]. Using
`find` would work as well if you're inclined that way.

[the silver searcher]: https://github.com/ggreer/the_silver_searcher


## Go get it!

If you want to try it out and automate your workflow a bit, there is now a
Fedora package, currently in waiting in Bodhi to be pushed stable (for [F23],
[F24], [F25] and [EPEL 7]). Feel free to get it from `updates-testing`
repository.

[F23]: https://bodhi.fedoraproject.org/updates/FEDORA-2016-6daf0bfef2
[F24]: https://bodhi.fedoraproject.org/updates/FEDORA-2016-b80a4445c2
[F25]: https://bodhi.fedoraproject.org/updates/FEDORA-2016-207ac2f3e3
[EPEL 7]: https://bodhi.fedoraproject.org/updates/FEDORA-EPEL-2016-0ca521c93f

```bash
$ sudo dnf install entr --enablerepo=updates-testing
```

[entr]: http://entrproject.org


Many thanks to Igor Gnatenko for review and packaging suggestions.
