#! /usr/bin/env bash

projectName="epub-tools"
version=$(awk '/^[Vv]ersion/ { print $2 }' "${projectName}.cabal")

buildDir="win-dist"

PREFIX="$buildDir" ./util/install.sh
install -Dm0644 doc/INSTALL "${buildDir}/usr/share/doc/${projectName}/INSTALL"

zipFile="${projectName}-${version}-win.zip"

rm $zipFile

cd $buildDir

zip -r ../$zipFile *
