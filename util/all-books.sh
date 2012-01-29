#! /bin/bash

# Copyright: 2008-2012 Dino Morelli
# License: BSD3 (see LICENSE)
# Author: Dino Morelli <dino@ui3.info>


cd /var/local/archive/doc/books/fiction

tempDir="/home/dino/temp"
allFile="epubname-all"

find . -name '*.epub' | xargs -n 30 epubname -n -o -v1 > $tempDir/$allFile

cd $tempDir

grep -v 'No-action' $allFile | grep -v 'formatter:' | sed -e 's/.*\///' | perl -ne '($o, $n) = /(.*) -> (.*)/; if ($o ne $n) { print $_ }' | sort > epubname-changed
