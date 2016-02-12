---
title: Today I Learned: Validating XML
tags: TIL, XML, validation
---

This is not rocket science nor brain surgery. I just never needed this and
found it practical to note this for further reference.

Setup: I have an XML file and a doctype, each in some place on my hard drive. I
want to check whether the document validates against the doctype.

```bash
xmllint --path ./path/to/dir/with/doctype path/to/my.xml --valid
```

If I don't want the formatted output (which I usually don't), there is an
option for that: `--noout` will suppress printing of the result tree.
