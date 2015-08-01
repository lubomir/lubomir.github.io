---
title: Reviewing GitHub pull requests locally
tags: GitHub
---

GitHub is a great place to collaborate on open-source code. One thing I have
struggled with a bit is how to perform code review for pull requests.

If the contributor has access to the actual repository and requests a pull
from a branch, it is really trivial. The real issue is with reviewing pull
requests coming from forks.

By reviewing, I don't mean just looking over the code. That could be done
through the web interface easy enough. Usually, I want to see the code in
action, play around with it a test it.

The way GitHub recommends right next to the merge button is to add another
remote and checkout a branch from it. This works, but it has the downside of
cluttering your local repo with a bunch of not so useful remotes.

Ideally, what I would like is to somehow just create one local branch with the
proposed changes.

Turns out GitHub allows just that. There is a ref pointing to the changes
created by the pull request. It is easy enough to create a local branch by the
following command:

```bash
git fetch origin pull/PULL_REQUEST_ID/head:LOCAL_BRANCH
```

Obviously, the parts in all caps need to be changed to something more useful.
After running the command, you can check out the local branch and do anything
you want with it. When you are done, simply delete it and everything is the
way it was before. It does not get much simpler than that.
