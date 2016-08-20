---
title: What's my next badge?
tags: Fedora, badges, Python
---

I love [Fedora Badges]. I'm not saying all I do is to get more badges, but it's
a great motivator. One thing that somewhat miss a guidance on what options I
have, what should I do to get another badge, how much activity will it need.

[Fedora Badges]: http://badges.fedoraproject.org/

Fedora Project is not the only community that awards badges to its members. For
example Stack Overflow has badges as well. In you Stack Overflow profile you
can see which badge you are likely going to get next and how much progress you
have made on that.

![Badges on Stack Overflow](/images/so-badges.png)

Is it possible to do something like this for Fedora badges? Turns out it
kind-of is. There actually is [a related issue] filed for the awesome [Fedora
Hubs project] to show options of next badges.

[an issue]: https://pagure.io/fedora-hubs/issue/160
[Fedora Hubs project]: https://pagure.io/fedora-hubs


All this actually relies on having information about [badge paths], but until
it's available in production, it can be reasonably hacked based on badge name
and a short list of exceptions.

[badge paths]: https://github.com/fedora-infra/tahrir/issues/343

## Baby steps

First thing that comes to mind is to simply look at statistics of the badges.
The ones that are awarded more often are most likely the easiest to get. Let's
start from that by finding the 5 most common badges that I don't have yet.

However, just taking that is not a particularly good suggestion: in my case 3
out of these badges turn out to be "have FAS account for at least X years". I'm
slowly getting there, but there's not much I can do to speed this up. It makes
sense to only show the first badge from each series.

<p style="text-align: center">
<a href="https://badges.fedoraproject.org/badge/dont-call-it-a-comeback" title="Don't Call it a Comeback – You reactivated your Fedora account"><img width=128 src="https://badges.fedoraproject.org/pngs/dont-call-it-a-comeback.png" alt="dont-call-it-a-comeback icon"></a>
<a href="https://badges.fedoraproject.org/badge/egg" title="Egg – You've had a FAS account for over one year"><img width=128 src="https://badges.fedoraproject.org/pngs/fas-account-egg.png" alt="egg icon"></a>
<a href="https://badges.fedoraproject.org/badge/curious-penguin-ask-fedora-i" title="Curious Penguin (Ask Fedora I) – You had your first interaction with the Ask Fedora forum system."><img width=128 src="https://badges.fedoraproject.org/pngs/askbot-curious-penguin-01.png" alt="curious-penguin-ask-fedora-i icon"></a>
<a href="https://badges.fedoraproject.org/badge/crypto-panda" title="Crypto Panda – You updated your GPG key with the Fedora Account System (FAS)"><img width=128 src="https://badges.fedoraproject.org/pngs/fas-crypto-panda.png" alt="crypto-panda icon"></a>
<a href="https://badges.fedoraproject.org/badge/speak-up!" title="Speak Up! – Participated in an IRC meeting."><img width=128 src="https://badges.fedoraproject.org/pngs/irc-speak-up.png" alt="speak-up! icon"></a>
</p>


## Progress towards next badge

Before we can estimate progress on getting a badge, it is important to
understand how the badges are awarded. The system is based on the messaging
bus^[Well, almost. Some badges are awarded manually. We can ignore that here.].
The [fedbadges] service listens to the bus and every time it sees a message, it
checks it against the rules it has defined.

The process starts with a simple check on the message content to make sure that
the message is connected to some badge. If it is, more complex checks are
performed. These checks either communicate with [pkgdb] or [datanommer], the
service that archives old messages.

[fedbadges]: https://github.com/fedora-infra/fedbadges
[datanommer]: https://github.com/fedora-infra/datanommer
[pkgdb]: https://admin.fedoraproject.org/pkgdb/

Now obviously I'm not keen on reimplementing the whole rule engine.
Fortunately, it is possible to reuse the code from [fedbadges]. All badges
that I care about for this part only need the datanommer integration, so that
is a big help.

The biggest issue I faced with this is the fact that [fedbadges] connects
directly to [datanommer]'s database. I can't do that. My workaround was to
write a script that would download all messages related to me from
[datagrepper] and store them locally. This works reasonably well for me
personally, but trying to get the messages for someone who is active for a
longer time is going to be an issue.

[datagrepper]: https://apps.fedoraproject.org/datagrepper/

Unfortunately, the list of messages related to a particular user is not enough
for all badges: [Bodhi] has badges for other people voting on your updates.
Therefore we also need all messages related to updates a person creates.

[Bodhi]: https://bodhi.fedoraproject.org/

<div style="text-align: center; margin-bottom: 1em">
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/senior-badger-badger-ii" title="Senior Badger (Badger II) – You got (about) 50 Fedora Badges.  Way to go!" style="display: block"><img src="https://badges.fedoraproject.org/pngs/badger-02.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 94.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">47 / 50</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/chocolate-chip-cookie-ii" title="Chocolate Chip (Cookie II) – Ten people think you're great, because they've given you cookies. Yummy!" style="display: block"><img src="https://badges.fedoraproject.org/pngs/chocolatechipcookie.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 80.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">8 / 10</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/bloggin-it!-planet-iii" title="Bloggin' it! (Planet III) – You posted 10 or more things to the Fedora Planet!" style="display: block"><img src="https://badges.fedoraproject.org/pngs/planet-bloggin-it-03.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 60.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">6 / 10</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/catching-the-bull-tester-iv" title="Catching the Bull (Tester IV) – *(a great struggle, repeated escapes, discipline required)* Tested and added karma to 20 updates-testing updates in Bodhi. " style="display: block"><img src="https://badges.fedoraproject.org/pngs/tester-04.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 55.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">11 / 20</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/what-goes-around-comes-around-karma-iii" title="What goes around comes around (Karma III) – You have received 50 or more good karma on your bodhi updates" style="display: block"><img src="https://badges.fedoraproject.org/pngs/what-goes-around-comes-around-50.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 50.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">25 / 50</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/building-the-outer-ring-copr-build-vii" title="Building the Outer Ring (Copr Build VII) – Successfully completed 999 copr builds" style="display: block"><img src="https://badges.fedoraproject.org/pngs/copr-build-999.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 43.8438438438%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">438 / 999</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/what-goes-up...-koji-failure-ii" title="What goes up... (Koji Failure II) – Submitted 5 or more koji builds that failed to build." style="display: block"><img src="https://badges.fedoraproject.org/pngs/koji-what-goes-up-02.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 40.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">2 / 5</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/you-can-call-me-patches-scm-iv" title="You can call me &quot;Patches&quot; (SCM IV) – Pushed 400 commits to a Fedora package repository." style="display: block"><img src="https://badges.fedoraproject.org/pngs/you-can-call-me-patches-04.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 36.5%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">146 / 400</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/if-you-build-it...-koji-success-iv" title="If you build it... (Koji Success IV) – Succesfully completed 250 koji builds." style="display: block"><img src="https://badges.fedoraproject.org/pngs/koji-if-you-build-it-04.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 34.8%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">87 / 250</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/is-this-thing-on-updates-testing-iii" title="Is this thing on? (Updates-Testing III) – Pushed 100 bodhi updates to the testing repositories." style="display: block"><img src="https://badges.fedoraproject.org/pngs/is-this-thing-on-03.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 34.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">34 / 100</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/like-a-rock-updates-stable-iii" title="Like a Rock (Updates-Stable III) – Pushed 100 bodhi updates to the stable repositories." style="display: block"><img src="https://badges.fedoraproject.org/pngs/like-a-rock-03.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 24.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">24 / 100</div>
</div>
</div>
<div style="display: inline-block; text-align: center">
<a href="https://badges.fedoraproject.org/badge/science-kernel-tester-ii" title="Science (Kernel Tester II) – You completed 5 runs of the kernel regression test suite" style="display: block"><img src="https://badges.fedoraproject.org/pngs/kernel_tester_5.png" width="128"></a>
<div style="width: 200px; height: 24px; background: #999; display: inline-block; color: #fff; position: relative;">
<div style="width: 20.0%; height: 100%; background: #3C6EB4; position: absolute;"></div>
<div style="position: absolute; left: 0; right: 0; text-align:center; line-height: 24px; font-weight: bold;">1 / 5</div>
</div>
</div>
</div>

Another problematic badge is the Cookie one: the number of cookies you have
get's reset every release cycle, so despite the number 8 above, I actually only
have 1 right now.


## I want it too

If you want to experiment with this code, I put it on Pagure as
[my-next-badge]. There are instructions in README. I didn't try to optimize
this in any way (yet), so it needs a lot of memory as all the messages must fit
there. In my case, it is about 40 MiB. For other people, it might be
significantly more. By "more" I mean it can easily be several gigabytes.

[my-next-badge]: https://pagure.io/my-next-badge

If you decide to try this, please note that the script is a hack that may not
always be correct. It may try to convince you that you satisfy conditions for
some badge even if you don't have it. Take it with a grain of salt.
