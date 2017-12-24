#! /bin/bash

version=$(awk '/^[Vv]ersion/ { print $2 }' epub-tools.cabal)

buildDir="dist"

./util/install.hs -p $buildDir -t bundle

zipFile="epub-tools-$version-win.zip"

rm $zipFile

cd $buildDir

zip -r ../$zipFile *
