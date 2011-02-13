#! /bin/bash

if [ -z "$1" ]
then
   echo "ERROR: Please give basename for zip file"
   echo "example: ./util/win-dist.sh epubname-2.3.1-win"
   exit 1
fi

zipFile="$1.zip"

rm $zipFile
zip -j $zipFile doc/INSTALL dist/build/epubname/epubname.exe
