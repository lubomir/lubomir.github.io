#!/bin/bash

set -e

if [ "$1" == "--recompile" ]; then
    echo "*** Compiling Hakyll ***"
    cabal configure
    cabal build
fi

echo "*** Building site ***"
./dist/build/site/site rebuild

if ! grep -q '<span class="dt">' _site/posts/2010-12-19-const.html; then
    echo " Missing syntax highlighting, exiting ..."
    exit 1
fi

echo "*** Syncing to server ***"
rsync -vrp --checksum _site/ xsedlar3@aisa.fi.muni.cz:/home/xsedlar3/public_html

echo "*** Creating git tag ***"
git tag -f -a "published" -m "$(date +"%Y-%m-%d")"
