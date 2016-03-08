---
title: Poor Man's CI
tags: Fedora, Pagure, Fedmsg, Jenkins, CI, testing
---

I have been using [Pagure] quite intensively recently and the one thing I miss
the most compared to GitHub is some sort of continuous integration. I just want
to see the green check appear on my pull requests.

[Pagure]: https://pagure.io/

There is a not-very-advertised [Jenkins server] used for Fedora Infrastructure.
The project I'm working on primarily, [Pungi], has a couple unit tests that are
run there after every push to `master` or some other long-lived branch.

[Jenkins server]: https://fedoraproject.org/wiki/Jenkins@infra
[Pungi]: https://pagure.io/pungi

This is a nice start, but leads to problems when new contributors submit their
pull requests. Unless someone runs the tests manually, we won't know if they
break.

As it turns out, it is quite possible to roll your own service to integrate the
two.


## Apply mutagen to Jenkins

Step one on my journey to having CI was to customize the Jenkins job definition
to be able to merge pull requests from any repository.

To be able to do so, I added two parameters. One is for the URL to the remote
repository, the other for the branch.

![Jenkins Parameters](/images/poormanci/parameters.png)

These parameters are available as environment variables in the script that runs
the tests. This snippet will add the repo with changes to be tested as a remote
and merge the proposed branch on top of master.

```bash
if [ -n "$REPO" -a -n "$BRANCH" ]; then
    git remote rm proposed || true
    git remote add proposed "$REPO"
    git fetch proposed
    git checkout origin/master
    git merge --no-ff "proposed/$BRANCH" -m "Merge PR"
fi
```

Last part of the Jenkins change is allowing remotely triggered builds. To do
this, it is necessary to set up an authentication token. Triggering a build is
then a simple HTTP `POST` request with a 
`?cause=203&REPO=https://example.com/repo.git&BRANCH=merge-this-plz&token=BEEFCAFE`.

The `cause` part is used to keep track of the pull request in question. It will
come in handy later.

Since we need to get information about the builds in some way, it is necessary
to enable sending messages to Fedmsg as a post-build action.


## Supercharge Pagure configuration

The configuration on Pagure side is not that complicated. All that needs to be
done is enable Fedmsg integration and set up an API key. This key needs to be
able to post comments to pull requests.


## One integration point to bind them all


The last part is to run a service that will listen to the messaging bus and
trigger actions as needed. When it hears about new or changed pull request, it
should trigger a build in Jenkins. When a build finishes (and is actually a
build that verified a pull request), a comment with details should be posted to
proper place.

I put the code to do exactly this into a [poor-man-ci] repo on Pagure.

[poor-man-ci]: https://pagure.io/poor-man-ci

I have deployed this thing on my VPS and so far it seems to be working quite
well. Open infrastructure for the win!


## Implementation details

The service is based on the [pdc-updater] model. There is a consumer running as
part of `fedmsg-hub`. It subscribes to three topics: one from Jenkins, one for
new pull requests on Pagure and one for comments on existing pull requests.

[pdc-updater]: https://github.com/fedora-infra/pdc-updater

As far as I could tell, Pagure does not send a separate message when pull
request is updated or rebased. It does however add a comment with this
information.

When a message from Pagure is received, the contents must be examined to
determine if a build should be triggered. For new pull requests this is
completely straightforward. For the update part we need to check if the last
comment is one of the known strings and if the pull request is still open.

Messages from Jenkins are a lot simpler. In fact they don't contain pretty much
anything but the build number. This is where [python-jenkins] comes into play.
It is a library on top of Jenkins API which makes it relatively easy to get
more details about the build. The only part I was interested in was the result
(this is actually in the message directly) and the note with pull request
number.

[python-jenkins]: http://python-jenkins.readthedocs.org/en/latest/

Finally the last part is to post a comment to Pagure. This is really trivial,
just submit an HTTP `POST` request.


## Deploying

Implementation wise, the hardest part for me was to package the whole service
and get it running. During most of the development I was using the
`fedmsg.tail_messages()` function, which was probably not meant for this use
case.

Ultimately, I just ripped of existing projects, mostly [pdc-updater] and
[the-new-hotness]. That helped some, but there are still a couple open
questions. For example, I have no idea why the packages do not depend on
`fedmsg-hub` when they clearly need it to run.

[the-new-hotness]: https://github.com/fedora-infra/the-new-hotness

My take away points for developing with Fedmsg are these:

**Point 1**: Your `setup.py` must specify an entrypoint for *moksha*.

```ini
[moksha.consumer]
integrator = poormanci.consumer:Integrator
```

This will get installed together with egg-info and the hub will somehow pick
it up. The name of the key is not important. The value should point to a
Python module containing a subclass of the `FedmsgConsumer`.

**Point 2**: The consumer class should have `topic` attribute listing all the
topics you are interested in, a `config_key` attribute with the name of the
configuration key that controls whether this consumer is enabled and finally a
`consume` method that will be called with the message received.

**Point 3**: The configuration file needs to live in `/etc/fedmsg.d`. Its name
is not important. It must be a Python module that defines a single value
`config` as a dict. It should contain at least the configuration code mentioned
above to enable the consumer, but can have arbitrary other stuff. I put the API
keys there, for example.

**Point 4**: The actual packaging (as in how to install the software) is not
really important as long as the module with the consumer can be imported and
the egg-info directory gets install into Python site lib.

**Point 5**: I had to restart the hub to have it pick up updates. This is as
easy as `systemctl restart fedmsg-hub.service`.

**Point 6**: I found it helpful to tail the hub logs to see what is happening –
`journalctl -f -u fedmsg-hub.service`. This log also contains traceback when
the consumer crashes.
