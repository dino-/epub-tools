#! /usr/bin/env bash

set -e

projectName="epub-tools"
version=$(awk '/^[Vv]ersion/ { print $2 }' "${projectName}.cabal")

PREFIX=${PREFIX:-"${projectName}-${version}"}
docDir="$PREFIX/usr/share/doc/${projectName}"
licenseDir="$PREFIX/usr/share/licenses/${projectName}"

stack install --local-bin-path="$PREFIX/usr/bin"
install -Dm0644 "LICENSE" "$licenseDir/LICENSE"
install -Dm0644 "README.md" "$docDir/README.md"
install -Dm0644 "changelog.md" "$docDir/changelog.md"
