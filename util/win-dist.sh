#! /bin/bash

if [ -z "$1" ]
then
   echo "ERROR: Please give basename for zip file"
   echo "example: ./util/win-dist.sh epub-tools-1.0.0.0-win"
   exit 1
fi

zipFile="$1.zip"

rm $zipFile

buildDir="dist/build"
zip -j $zipFile doc/INSTALL $buildDir/epubmeta/epubmeta.exe $buildDir/epubname/epubname.exe $buildDir/epubzip/epubzip.exe
