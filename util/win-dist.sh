#! /bin/bash

version=$(awk '/^[Vv]ersion/ { print $2 }' epub-tools.cabal)

buildDir="dist/build"

binaries="$buildDir/epubmeta/epubmeta.exe $buildDir/epubname/epubname.exe $buildDir/epubzip/epubzip.exe"

strip $binaries

zipFile="dist/epub-tools-$version-win.zip"

rm $zipFile

zip -j $zipFile doc/INSTALL $binaries
