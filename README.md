# epub-tools


## Synopsis

Command line utilities for working with epub files (Haskell)


## Description

A suite of command-line utilities for creating and manipulating epub book files. Included are: epubmeta, epubname, epubzip. This software uses the epub-metadata library, also available on Hackage.

### epubmeta

epubmeta is a command-line utility for examining and editing epub book metadata. With it you can export, import and edit the raw OPF Package XML document for a given book. Or simply dump the metadata to stdout for viewing in a friendly format.

Here's an example of epubmeta output:

      $ epubmeta Kelly_Kessel_Lethem-NinetyPercentOfEverything.epub

      package
         version: 2.0
         unique-identifier: calibre_id
      identifier
         id: calibre_id
         scheme: calibre
         text: b1026732-69a5-4a05-a8d9-a1701685f6fa
      identifier
         scheme: ISBN
         text: 1-590620-00-3
      title: Ninety Percent of Everything
      language: en-us
      contributor
         text: calibre (0.5.1) [http://calibre.kovidgoyal.net]
         file-as: calibre
         role: bkp
      creator
         text: James Patrick Kelly
         file-as: Kelly, James Patrick
         role: aut
      creator
         text: John Kessel
         role: aut
      creator
         text: Jonathan Lethem
         role: aut
      date: 2001-03-25T00:00:00
      publisher: www.Fictionwise.com
      subject: Science Fiction/Fantasy

### epubname

epubname is a command-line utility for renaming epub ebook files based on their OPF Package metadata. It tries to use author names and title info to construct a sensible name.

Using it looks like this:

      $ epubname poorly-named-book.epub

      poorly-named-book.epub -> WattsPeter-Blindsight_2006.epub

      $ epubname another-poorly-named-book.epub

      another-poorly-named-book.epub -> Kelly_Kessel_Lethem-NinetyPercentOfEverything.epub

### epubzip

epubzip is a handy utility for zipping up the files that comprise an epub into an .epub zip file. Using the same technology as epubname, it can try to make a meaningful filename for the book.


## Getting this software

Binaries

- epub-tools is available for Arch Linux [from the AUR](http://aur.archlinux.org/packages/epub-tools/)
- Download binary distributions for generic Linux, OSX and Windows [from this directory](https://github.com/dino-/epub-tools/releases/download/4.0)


Getting source for development

- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/epub-tools)
- Get the source with git: `$ git clone https://github.com/dino-/epub-tools.git`
- Get the source with stack: `$ stack unpack epub-tools`
- If you're just looking, [browse the source](https://github.com/dino-/epub-tools)


Once you have source, building the usual way:

      $ stack build
      $ stack test
      $ stack haddock


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
