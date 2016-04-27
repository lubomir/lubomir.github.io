---
title: Git hooks for required sign-off
tags: git, Fedora, bash
---

There are a couple projects out there ([rpkg], [pungi] etc.) that require each commit
to have a signed-off-by line. [Pagure] even has a setting that will make it
impossible to merge commits without it.

[pungi]: https://pagure.io/pungi
[rpkg]: https://pagure.io/rpkg
[Pagure]: https://pagure.io/

As far as I know, it is supposed to indicate that whoever submitted the commit
agrees with the [Developer Certificate of Origin].

[Developer Certificate of Origin]: http://developercertificate.org/

It is easy to add the line to the commit. Git even has a command line option
for it: `-s`.

It is even easier to forget it, though. But thankfully computers are pretty
good at being consistent, so I wrote a few git hooks to remind me.

There are [a couple client-side hooks][1] available. Originally the goal was to
use `commit-msg` to reject the commit completely. This turned out to be really
frustrating: I would write a nice descriptive commit message only to be told
*“you forgot `-s`, write it all again”*. Not cool.

[1]: https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks

Instead I modified to hook to still nag me, but allow me to commit
none-the-less. After all, during frantic coding it is often useful to be able
to make quick commits, often with the message only saying *WIP*.

The example `commit-msg` hook included a check for a duplicate sign-off line,
but is never a problem for me, so I don't use that.

```bash
#!/bin/sh
RED=""
NORMAL=""
if [ -f ~/.term_colors ]; then
    . ~/.term_colors
fi

if [ "1" != "$(grep -c '^Signed-off-by: ' "$1")" ]; then
    printf >&2 "%sMissing Signed-off-by line.%s\n" "$RED" "$NORMAL"
fi
```

The sourced `.term_colors` file is available in [my dotfiles][2]. It only
really contains a bunch of variables definitions with ANSI color sequences.

[2]: https://github.com/lubomir/dotfiles/blob/master/term_colors

With this hook I get reminded directly after the commit. To make sure I don't
push anything breaking rules there is a `pre-push` hook. This one actually
aborts the push if anything is wrong. It can however be bypassed with
`--no-verify` option.

It is inspired by sample `pre-push` hook that rejects pushing commits whose
summary starts with *WIP*.

```bash
#!/bin/bash

# shellcheck disable=2034
remote="$1"
# shellcheck disable=2034
url="$2"

z40=0000000000000000000000000000000000000000

# shellcheck disable=2034
while read local_ref local_sha remote_ref remote_sha
do
    if [ "$remote_ref" = "refs/heads/master" ]
    then
        # Any push to master is ok
        continue
    fi

    if [ "$local_sha" = $z40 ]
    then
        # Branch is deleted, nothing to check here, move along.
        :
    else
        if [ "$remote_sha" = $z40 ]
        then
            # New branch, examine all commits
            range="$local_sha"
        else
            # Update to existing branch, examine new commits
            range="$remote_sha..$local_sha"
        fi

        # Check for WIP commit
        commit=$(git rev-list -n 1 --grep '^WIP' "$range")
        if [ -n "$commit" ]
        then
            echo >&2 "Found WIP commit in $local_ref, not pushing"
            exit 1
        fi

        # Check for commits without sign-off
        if [ "$remote_sha" = $z40 ]; then
            # New branch is pushed, we only want to check commits that are not
            # on master.
            range="$(git merge-base master "$local_sha")..$local_sha"
        fi
        while read ref; do
            msg=$(git log -n 1 --format=%B "$ref")
            if ! grep -q '^Signed-off-by: ' <<<"$msg"; then
                echo >&2 "Unsigned commit $ref"
                exit 1
            fi
        done < <(git rev-list "$range")
        # The process substitution above is a hack to make sure loop runs in
        # the same shell and can actually exit the whole script.
    fi
done

exit 0
```
