---
title: Clickable Pungi logs
tags: Fedora, Pungi, Java Script, logs
---

When debugging problems with composes, the logs left behind by all stages of
the compose run are tremendously helpful. However, they are rather difficult to
read due to the sheer volume. Being exposed to them quite intensively for close
to a year helps, but it still is a nasty chore.

The most accessible way to look at the logs is via a web browser on [kojipkgs].
It's just *httpd* displaying the raw log files on the disk.

[kojipkgs]: https://kojipkgs.fedoraproject.org/compose/rawhide/

It took me too long to figure out this could be made much more pleasant that
copy-pasting stuff from the wall of text.

How about a user script that would run in [Greasemonkey] and allow clicking
through to different log files or even Koji tasks?

![Is this not better?](/images/kojipkgs-screenshot.png)

[Greasemonkey]: http://www.greasespot.net/

Turns out it's not that difficult.

Did you know that when Firefox displays a `text/plain` file, it internally
creates an HTML document with all the content in one `<pre>` tag.

The whole script essentially just runs a search and replace operation on the
whole page. We can have a bunch of functions that take the whole content as
text and return it slightly modified.

First step will make URLs clickable.

```javascript
function link_urls(str) {
  let pat = /https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)/g;
  return str.replace(pat, '<a href="$&">$&</a>');
}
```

I didn't write the crazy regular expression myself. I got from [Stack
Overflow].

[Stack Overflow]: http://stackoverflow.com/a/3809435/1576064

Next step can make paths to other files in the same compose clickable.

```javascript
function link_local_files(url, pathname, mount, str) {
  let pat = new RegExp(mount + pathname + '(/[^ ,"\n]+)', 'g');
  return str.replace(pat, function (path, file) {
    return '<a href="' + url + file + '">' + path + '</a>';
  });
}
```

The last thing left is not particularly general: linking Koji tasks
identifiers.

```javascript
function link_tasks(taskinfo, str) {
  return str.replace('\d{8,}/m', '<a href="' + taskinfo + '$&">$&</a>')
            .replace(/(Runroot task failed|'task_id'): (\d{8,})/g,
                     '$1: <a href="' + taskinfo + '$2">$2</a>);
  }
}
```

Tying all these steps together and passing in the extra arguments is rather
trivial but not very generic.

```javascript
window.onload = function () {
  let origin = window.location.origin;
  let pathname = window.location.pathname.split('/', 4).join('/');
  let url = origin + pathname;
  let taskinfo = 'https://koji.fedoraproject.org/koji/taskinfo?taskID=';
  let mount = '/mnt/koji';

  var content = document.getElementsByTagName('pre')[0];
  var text = content.innerHTML;
  content.innerHTML = link_local_files(
    url, pathname, mount,
    link_tasks(taskinfo, link_urls(text))
  );
}
```

If you find this useful, feel free to grab [the whole script with a header].

[the whole script with a header]: /data/clickable-kojipkgs.user.js
