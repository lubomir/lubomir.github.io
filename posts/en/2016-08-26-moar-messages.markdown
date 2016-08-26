---
title: Using fedmsg to send more fedmsg
tags: Fedora, fedmsg, Python
---

Fedmsg is a perfect system for automating things. I mostly use it to integrate
[Pagure] with [Jenkins], which works rather well.

[Pagure]: https://pagure.io/
[Jenkins]: http://jenkins.fedorainfracloud.org/

One hiccup is that when a pull request is updated (by pushing more commits to
the branch or rebasing it), Pagure will not notice that until someone visits
the pull request page. At that point a comment with the notice is generated and
the message sent.

My first instinct to automate this was to use a git hook. It's easy to automate
hitting each pull request page.

```bash
curl -s https://pagure.io/api/0/pungi/pull-requests | \
    jq '.requests[] | select(.user.name == "lsedlar") | .id' | \
    while read PR_ID; do
        echo "https://pagure.io/pungi/pull-request/$PR_ID"
        curl -s "https://pagure.io/pungi/pull-request/$PR_ID" >/dev/null
    done
```

However, there [is no `post-push`
hook](http://stackoverflow.com/a/9038644/1576064), so one would have to run the
script manually. That's too much work.


## Fedmsg to the rescue

How about getting notified about the push from the server? Pagure will send a
message announcing a push immediately.

What we need is a very simple consumer that waits for pushes, finds where the
pull requests would likely be going to, tries to find one from this branch and
hits the web page.

If the push is to a fork, we can assume the pull request would be filed against
the original repo. If it's not a fork, then the pull request would probably be
against the same repo.

```python
BASE_URL = 'https://pagure.io'


class Poker(fedmsg.consumers.FedmsgConsumer):
    topic = ['io.pagure.prod.pagure.git.receive']
    config_key = 'poker.enabled'

    def consume(self, msg):
        msg = msg['body']['msg']
        branch = msg['branch'].split('/')[-1]
        repo = msg['repo']['name']
        if msg['repo']['parent']:
            repo = msg['repo']['parent']['name']

        url = '%s/api/0/%s/pull-requests' % (BASE_URL, repo)
        response = requests.get(url)
        if response.status_code != 200:
            self.log.warning('Failed to get %s ...', url)
            return

        for pr in response.json()['requests']:
            if pr['repo_from']['id'] != msg['repo']['id']:
                continue
            if pr['branch_from'] != branch:
                continue
            url = '%s/%s/pull-request/%s' % (BASE_URL, repo, pr['id'])
            self.log.info('Poking %s', url)
            requests.get(url)
            return
```

A file with this class should be installed with `setup.py` somewhere under
`site-packages`. For `fedmgs-hub` to pick the consumer up, there must be a
special section in the `setup()` function call:

```python
entry_points="""
[moksha.consumer]
pokeapagure = poke_a_pagure:Poker
"""
```

The name on the left of the equals sign does not really matter. The string on
the right should be a package name and a class name of the consumer. In this
case the file will be `poke_a_pagure/__init__.py`.


## How to make it work

This consumer will run as part of `fedmsg-hub` (provided in similarly named
package). All that is needed to get it working is to install the hub and the
python package with the class.

There also needs to be a config file in `/etc/fedmsg.d/poke_a_pagure.py`. The
name is completely arbitrary, nothing depends on it.

```python
config = {
    'poker.enabled': True
}
```

Note the matching config key with the consumer above. There can be more
configuration, such as custom rules for logging.

Once that exists, just restart the hub and observe its logs. It will tell you
which consumers were loaded.

It's this easy to write a consumer of messages. Now, would it be easier to fix
Pagure to send the message immediately? Maybe. But is working with fedmsg fun?
Definitely!
