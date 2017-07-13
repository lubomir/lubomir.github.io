---
title: A compose what?
tags: Fedora, rel-eng, compose
---

Kushal wrote [a nice post about how package updates][0] get into repositories.
However he glosses over rawhide by saying it happens automatically. This is
true, but I want to elaborate on the mechanism of how it happens..

[0]: https://kushaldas.in/posts/story-of-mashing-in-bodhi-fedora-updates-system.html

A package build for Rawhide is not available in the repositories immediately.
It has to wait until a nightly Rawhide compose runs that updates the repo.

Let's take a dive into what exactly the compose does and is.


## Terminology

A compose is a snapshot of a release with a specific identity (which is
determined by the actual release and date when it was created, like
`Fedora-Rawhide-20170710.n.0`). It consists of at least one variant (such as
`Server`, `Workstation` or `Everything`). A variant is a subset of the content
from a release that is aimed for particular use case.

Generally a variant in the final compose contains a repo with RPMs and other
artefacts like images of various kinds.

The compose is created by a tool called [Pungi]. Current major version 4 is
significantly different to previous version in that it integrates with Koji and
delegates as much work as possible to it (for better tracking and
reproducibility).

[Pungi]: https://pagure.io/pungi

The main input for Pungi is [a configuration file][1] that can get quite
complicated, due to all the extra artifacts like images. I'm only going to
focus on the most common use case here.

[1]: https://pagure.io/pungi-fedora


## Getting the packages

The process for defining what packages will go into the compose actually
consists of two steps. First we need to find out the latest builds in Koji and
find appropriately signed copies of the packages on the local file system (we
need to have the same volume that Koji uses mounted locally).

For nightly composes of Rawhide or branched the appropriate `f2X` tag is used.
In milestone composes (where packages with freeze exception or fixes for
accepted blockers are supposed to get in), there's a special `f2X-compose` tag
that inherits from `f2X` and this way other packages can get into the compose.

```bash
$ koji list-tag-inheritance f26-compose
f26-compose (368)
  └─f26 (357)
```

The second step is to determine which packages are supposed to go into each
variant. Each [variant defines a list of comps groups][2]. Packages from those
groups will be pulled in together with all of their dependencies. This process
needs to run once for each architecture on each variant.

[2]: https://pagure.io/pungi-fedora/blob/master/f/variants-fedora.xml

There are of course exceptions here: additional packages not in comps can be
pulled in, and also there are [configuration options][3] to customize multilib
rules and tweak other things.

[3]: https://docs.pagure.org/pungi/configuration.html#gather-settings


## Creating repositories

Once we have the package lists, we can call `createrepo_c` and create the
actual metadata. The list of packages is actually part of the metadata in the
final compose. Usually it's a really big file located in
`compose/metadata/rpms.json` (which is way too big to link here, but you can
read [description of the format][4]).

[4]: http://productmd.readthedocs.io/en/latest/rpms-1.1.html


## Building images

There are multiple different images that can be built as part of the compose.
Most work starts by calling [lorax] to create `boot.iso` and boot configuration
files. This image is directly used as the *netinstall* media.

[lorax]: https://github.com/rhinstaller/lorax

Pungi can then create DVD images that include the repository. In Fedora this is
currently only used for *Server* variant. This is done by basically taking the
`boot.iso` and adding additional stuff to it.

Spins and Labs are created in Koji using `livemedia-creator` (a `livemedia`
task). This takes additional input of [a kickstart file][5] defining how that
particular image should be created. The packages that are installed are taken
from the repos created in the compose.

[5]: https://pagure.io/fedora-kickstarts


## How is it triggered?

The nightly compose is started by a cron job that clones the latest
configuration, runs the compose, and if it finishes successfully, it syncs the
content to mirrors and sends e-mails about changes in packages.

Branched composes have the same nightly process. Milestone composes are created
in a similar way, but are started manually (because they require label via a
command line argument).


## Conclusion

That concludes the high level overview. [The official documentation][6] will go
into a bit more detail once Pungi 4.1.17 is released.

[6]: https://docs.pagure.org/pungi/
