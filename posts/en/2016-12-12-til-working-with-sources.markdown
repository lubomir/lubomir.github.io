---
title: Today I Learned: Handling source tarballs
tags: Fedora, Koji, process
---

Fedora packages are generally built from tarballs provided by upstream
developers. What's actually happening to these tarballs and how do they move
around?


## How are sources uploaded?

When a new tarball is supposed to be used, the packager will upload it to
lookaside cache (because storing big binary files directly in git is bad). This
is done with `fedpkg upload` or `fedpkg new-sources`. These commands make a
`POST` request to a CGI script running on the dist-git server. This requests
includes the name of the file, a checksum and the actual file contents.

Up until today (Dec 12) MD5 hash has been used. Now it's SHA512.

There are actually two requests each time an upload is requested: the first one
is to check if a file with the same name and hash has been uploaded already. If
it is, there's no point doing it again.


## How does [fedpkg] download them?

Downloading back to development machine is done by `fedpkg sources`. It reads
the `sources` file in the git repository and requests the files from lookaside
cache. The URL for the files contains both filename and the hash.

Once the file is downloaded, the hash is verified to make sure we got the file
we asked for.


## How does Koji download them?

When a build is submitted to Koji, an SCM URL has to be specified. It needs to
point to commit you want to build. Koji will not allow you to build from
arbitrary locations though. There is a whitelist of allowed systems.

Once the spec and list of sources are downloaded, the actual source code needs
to be retrieved as well. Unless configuration specifies otherwise, Koji would
run `make sources`. For Fedora dist-git, the command is unsurprisingly
configured to be `fedpkg sources`.

However, since this command needs to run the buildroot and `fedpkg` has a
relatively big dependency footprint, the `fedpkg` command in buildroots is
provided by [fedpkg-minimal] package. This package contains a single executable
shell script that parses the `sources` file, downloads everything and verifies
the hashes.

[fedpkg]: https://pagure.io/fedpkg
[fedpkg-minimal]: https://pagure.io/fedpkg-minimal
