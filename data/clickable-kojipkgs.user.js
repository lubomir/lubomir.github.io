// ==UserScript==
// @name        Clickable links on kojipkgs
// @namespace   lsedlar
// @include     https://kojipkgs.fedoraproject.org/compose/*
// @version     1
// @grant       none
// ==/UserScript==

function link_local_files(url, pathname, mount, str) {
  let pat = new RegExp(mount + pathname + '(/[^ ,"\n]+)', 'g');
  return str.replace(pat, function (path, file) {
    return '<a href="' + url + file + '">' + path + '</a>';
  });
}


function link_urls(str) {
  let pat = /https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)/g;
  return str.replace(pat, '<a href="$&">$&</a>');
}

function link_tasks(taskinfo, str) {
  return str.replace(/^\d{8,}/m, '<a href="' + taskinfo + '$&">$&</a>')
            .replace(/(Runroot task failed:|'task_id':) (\d{8,})/g,
                     '$1 <a href="' + taskinfo + '$2">$2</a>');
}

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
