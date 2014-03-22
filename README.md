# epub-tools


## Synopsis

Command line utilities for working with epub files


## Description

A suite of command-line utilities for creating and manipulating epub book files. Included are: epubmeta, epubname, epubzip. This software uses the epub-metadata library, also available on Hackage.

### epubmeta

epubmeta is a command-line utility for examining and editing epub book metadata. With it you can export, import and edit the raw OPF Package XML document for a given book. Or simply dump the metadata to stdout for viewing in a friendly format.

Here's an example of epubmeta output:

>     $ epubmeta Kelly_Kessel_Lethem-NinetyPercentOfEverything.epub
>
>     package
>        version: 2.0
>        unique-identifier: calibre_id
>     identifier
>        id: calibre_id
>        scheme: calibre
>        text: b1026732-69a5-4a05-a8d9-a1701685f6fa
>     identifier
>        scheme: ISBN
>        text: 1-590620-00-3
>     title: Ninety Percent of Everything
>     language: en-us
>     contributor
>        text: calibre (0.5.1) [http://calibre.kovidgoyal.net]
>        file-as: calibre
>        role: bkp
>     creator
>        text: James Patrick Kelly
>        file-as: Kelly, James Patrick
>        role: aut
>     creator
>        text: John Kessel
>        role: aut
>     creator
>        text: Jonathan Lethem
>        role: aut
>     date: 2001-03-25T00:00:00
>     publisher: www.Fictionwise.com
>     subject: Science Fiction/Fantasy

### epubname

epubname is a command-line utility for renaming epub ebook files based on their OPF Package metadata. It tries to use author names and title info to construct a sensible name.

Using it looks like this:

>     $ epubname poorly-named-book.epub
>
>     poorly-named-book.epub -> WattsPeter-Blindsight_2006.epub
>
>     $ epubname another-poorly-named-book.epub
>
>     another-poorly-named-book.epub -> Kelly_Kessel_Lethem-NinetyPercentOfEverything.epub

### epubzip

epubzip is a handy utility for zipping up the files that comprise an epub into an .epub zip file. Using the same technology as epubname, it can try to make a meaningful filename for the book.


## Getting source

Get the source with darcs:

>     $ darcs get http://ui3.info/darcs/epub-tools

Or [browse the source](http://ui3.info/darcs/epub-tools)

And then building the usual way:

>     $ cabal configure --enable-tests
>     $ cabal build
>     $ cabal test


## Installing

epub-tools can be installed in several ways:

- Build and install with cabal-install:
  `$ cabal update ; cabal install epub-tools`
- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/epub-tools)
- Download the cabalized source tarball [from here](http://ui3.info/d/proj/epub-tools/epub-tools-2.4.tar.gz)
- epub-tools is available for Arch Linux [from the AUR](http://aur.archlinux.org/packages/epub-tools/)
- Download [binaries for Windows](http://ui3.info/d/proj/epub-tools/epub-tools-2.4-win.zip)


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
