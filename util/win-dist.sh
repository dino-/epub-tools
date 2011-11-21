#! /bin/bash

version=$(awk '/^[Vv]ersion/ { print $2 }' epub-tools.cabal)

buildDir="dist/build"

binaries="$buildDir/epubmeta/epubmeta.exe $buildDir/epubname/epubname.exe $buildDir/epubzip/epubzip.exe"

strip $binaries

rm $zipFile

zip -j dist/epub-tools-$version-win.zip doc/INSTALL $binaries
