---
title: Exploring test case fixtures in Django
tags: Python, Django
---

[Django] has a nice feature for exploring the environment in which the tests
run via the `testserver` command. You can give it paths to fixture files used
in the test case you want and go crazy.

However, this use case becomes really unpleasant once you need to load
bigger number of fixtures. It would be much more user friendly if you
could instead tell it what test case you want to explore and the script
automatically inferred what fixtures to load.

The `--help` output is silent on this, so let's roll our own solution
to this. The script will have two mandatory arguments: module
containing the tests and the actual test case name.

     run-test-server.py myapp.tests MyComplicatedTestCase

The first idea how to proceed is to simply import the module and look at
`fixtures` attribute of the test case. Sadly, it is not that easy. Importing
the module fails with `ImproperlyConfiguredException` because simply importing
a module would be too much to ask for without loading all the [Django]
settings. This could be a whole other rant.

Since `__import__` is slightly more complicated that necessary, let's offload
the actual importing to `importlib.import_module` which works exactly as
expected. However, it is only available since Python 2.7.

Once the module is imported, all that needs to be done is to retrieve
the test case, its fixtures and fire out the `testserver` command with
appropriate arguments.

The whole script looks as follows. The only unexplained part is the
modification of path. This needs to be done so that the script can be
located anywhere and still manage to import modules from current
directory.

``` python
#! /usr/bin/env python

import argparse
from subprocess import call
import sys
import importlib

from django.conf import settings


def get_fixtures(module, case):
    settings.configure()
    parts = module.split('.')

    app = importlib.import_module(module)
    return getattr(app, case).fixtures


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('MODULE', type=str)
    parser.add_argument('TEST_CASE', type=str)

    args = parser.parse_args()
    fixtures = get_fixtures(args.MODULE, args.TEST_CASE)

    call(["python", "manage.py", "testserver"] + fixtures)


if __name__ == '__main__':
    sys.path.append('.')
    main()
```

## Further improvements

This script could obviously use a bit more polish:

 * The help output could be more helpful.
 * If something breaks, the user is presented with a stack trace. Not
   nice.

[Django]: https://www.djangoproject.com/
