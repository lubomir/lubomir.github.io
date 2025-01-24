---
title: "Today I Learned: Converting between JSONL and JSON"
tags: json, json, jq, TIL
---

[jq](https://jqlang.github.io/jq/) is able to easily process jsonl as well as
regular json.

All you need to do is add `-s` option to read whole input and merge it all into
a large array.

For example:
```
$ curl -s https://kojipkgs.fedoraproject.org/repos/eln-build/6071552/x86_64/rpmlist.jsonl | jq -s '[.[] | .size] | add'
105368819406
```
