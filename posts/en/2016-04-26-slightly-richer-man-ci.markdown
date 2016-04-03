---
title: Slightly Richer Man's CI
tags: Fedora, Pagure, Jenkins, CI, testing
---

Not so long ago I have written about [my attempts to bring CI to
Pagure](/en/2016-03-08-poor-man-ci.html). It was pointed out to me that a
couple assumptions I've made are actually incorrect.

Here are the errata:

  * When a pull request is updated or rebased, there is no need to check the
    message body. [Pagure] already puts this information into the message (look
    for `notification: true`).

  * Using comments for indicating status is too clumsy, especially given the
    fact there is a feature designed to communicate exactly this type of
    information – flags. Setting a flag is pretty much the same as posting a
    comment, but they appear in a sidebar with a link and some text. You can
    also add a percentage to it that will determine the color of a badge.

  * The *Fedmsg* hook that you can enable in the project settings is actually
    not required. I misunderstood what it does. The notifications on new pull
    requests get send automatically without any change in configuration
    required.

    The hook is actually git `post-receive` hook that will send you tons of
    e-mail through the [Fedora notification] system whenever a commit is pushed
    to master without going through a pull-request.

[Pagure]: https://pagure.io/
[Fedora notification]: https://apps.fedoraproject.org/notifications

Anyway, fixing these is quite simple.

While the setup described in previous post worked fine for my use case, it was
not ideal. One of the biggest issues is the configuration: adding stuff to
`/etc` is not a scalable model. First order of business was to create a web
interface where the configuration could be managed. Added benefit: with a
decent authentication system (yes, FAS does nicely) it's possible for anyone to
configure their integration points.

![Screenshot of index page](/images/poormanci/index.png)


## Jenkins wants a voice too

Another big drawback is the dependency on Fedora Infrastructure Jenkins. While
setting up your own Jenkins is easy (approx. three clicks on [OpenShift]),
connecting it to production *Fedmsg* is probably not (I don't know, did not
try).

[OpenShift]: https://www.openshift.com/

Since there is now a web server for the interface, it is not a big step to
support web hooks. There is a [Notification plugin] for Jenkins that can ping a
URL with JSON data whenever a job finishes.

The real message contains a ton of data, but for this use-case only project
name and build number are really interesting. As long as this data is supplied,
we are happy.

```json
{
    "name": "asgard",
    "build": {
        "number": 1,
    }
}
```

[Notification plugin]: https://wiki.jenkins-ci.org/display/JENKINS/Notification+Plugin

If instead of web requests the plugin gives you stack traces, try setting log
lines to 1.

![Plugin configuration](/images/poormanci/notification.png)

(No, that is not an actual token in the shown URL. No need to try.)


## Here we are and there we go

The semi-finished service is available at <http://poormanci.lsedlar.cz/>. The
documentation really is lacking, though. The best guide how to use it is [the
previous blog post](/en/2016-03-08-poor-man-ci.html) or [a somewhat
work-in-progress help page](http://poormanci.lsedlar.cz/help).

Now there is still a ton of things to improve. Currently, all requests to
external services are sent directly from the fedmsg consumer or web app
process. Since these are blocking and could potentially take a long time, it's
a prime candidate for denial-of-service. I need to refactor this into a
separate worker process.

Another thing to add would be the support for a web hook sent from Pagure. This
would make it possible to use a custom instance. First I need to learn what
data is actually sent in the HTTP request.

Another point of improvement is the deployment of the whole thing. Currently, I
build everything [in COPR], install the RPM and do any database migrations
almost by hand. The initial setup was also manual. I plan to write an Ansible
playbook to make future deployments simpler. It will also document the process
a bit.

[in COPR]: https://copr.fedorainfracloud.org/coprs/lsedlar/poor-man-ci/

Next item on the list is support for multi-configuration projects in Jenkins.
The Matrix plugin in Jenkins allows a single test suite to run on multiple
builders (e.g. with different Python versions). I want to support such
configuration too, but the *Fedmsg* integration will not do here. [The
messages] from such builds are not particularly helpful. I have reported [the
issue] upstream, but I'm really in no position to go fix the Java code.

[The messages]: https://apps.fedoraproject.org/datagrepper/id?id=2016-ef190673-90c3-4947-b82f-a909d870d53f&is_raw=true&size=extra-large
[the issue]: https://github.com/fedora-infra/jenkins-fedmsg-emit/issues/3

The reporting from the notification plugin works well, though.
