#!/bin/bash

set -e

echo "*** Compiling Hakyll ***"
ghc --make hakyll.hs

echo "*** Building site ***"
./hakyll rebuild

echo "*** Syncing to server ***"
rsync -vrp --checksum _site/ xsedlar3@aisa.fi.muni.cz:/home/xsedlar3/public_html

echo "*** Creating git tag ***"
git tag -f -a "published" -m "$(date +"%Y-%m-%d")"
