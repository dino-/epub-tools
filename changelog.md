2.11 (2018-01-16)

  * Switched license from BSD3 to ISC
  * Fixes and additions to some epubname rules
  * Fixed a couple of cabal file issues


2.10 (2017-12-24)

  * Changed the Stackage resolver from a nightly to an lts
  * Now using a less-fragile `Printf*` type signature
  * Removed an unused GHC options compile directive
  * Added a missing module to all other-modules stanzas
  * Added a unit test for authors with a middle-name


2.9 (2016-10-19)

  * Switched build to stack
  * Added hsinstall installation script and updated windows dist script
  * Various cabal file updates
  * Moved copyright date up to 2016
  * Updated README with better instructions for getting this software


2.8 (2015-09-18)

  * Fixed some magazine and anthology naming rules


2.7 (2015-05-29)

  * Added back Control.Applicative import for GHC 7.8 compatibility


2.6 (2015-05-25)

  * Fixed an error in bad DSL command index reporting
  * Replaced deprecated Control.Monad.Error with
    Control.Monad.Except
  * Replaced deprecated System.Cmd with System.Process
  * Removed useless import of Control.Applicative
  * Updated cabal homepage, tested-with and source-repository
  * Removed unused path to rules file in unit tests
  * Reformatted TODO and development notes into Markdown documents
  * Updated boringfile with cabal sandbox filespecs
  * Switched over to using Data.Version instead of hard-coded
    version string


2.5 (2014-04-04)

  * Additions and modifications to the stock rules to both support
    more books and also use more generic rules than before
  * Added code to set proper case for Roman numerals in titles
  * Added code to handle file-as names with parenthesized name info
  * Simplified and consolidated special-character filtering code
  * Added support for multiline fields in the Metadata


2.4 (2014-03-19)

  * Fixed problem in Windows cmd shell with missing UNIX HOME
    env variable
  * Now gracefully handling last-name-first creators. For books
    with no file-as and the Creator text arranged last-name-first
    with a comma, do the right thing.
  * Added a new rule for generically-titled magazines with an issue
  * Incorporated project website info into README.md and
    changelog.md files. This information is now in source control
    where it belongs.
  * Added missing files to .cabal for sdist
  * Changed copyright date range to 2014


2.3 (2013-09-20)

  * These tools now support both epub2 and epub3
  * Documentation changes and additions


2.2 (2013-04-14)

  * Updated to build against recent changes and bug fixes in
    epub-metadata 3.0
  * All support data files have been brought into the binaries
    now. This makes these tools more tolerant to being moved to a
    different location than what they were configured for build with.
  * Some documentation additions and changes


2.1.1 (2013-02-03)

  * Fixed a stack overflow problem with some epub documents


2.1.0 (2013-02-01)

  * Added new subjectMatch command to the DSL, similar to
    authorMatch. This is being primarily used to detect anthology
    publications.
  * Removed some rules that are now handled by anthology detection,
    and fixed relevant unit tests
  * Clarified DSL documentation for authorMatch a little more
  * Modified rules for some magazines to reflect changes to
    recent editions
  * Fixed an error in the DSL documentation
  * Fixed a bug in epubzip where no epub file will be created if
    none already exists


2.0.0 (2012-10-31)

  * Major redesign of the formatting rules system. Renaming
    machinery is now described in a domain-specific language,
    NOT in statically compiled code. Users are able to extend the
    functionality with custom naming rules in conf files.
  * Added interactive mode to ask about each file rename as they
    happen, this is like darcs now!
  * Added ability to specify target directory for books to be
    moved to as part of renaming. Includes code to check that target
    directory exists.
  * Removed --overwrite option. Turns out, renameFile has always
    been smart enough to not overwrite existing.
  * Added / character to filters, a big no-no character for file
    paths on most sane filesystems
  * Publication year was looking for publication before
    original-publication, causing problems in books that have
    both tags
  * Miscellaneous rules changes for various publications


1.1.2 (2012-01-29)

  * Changed how this code provides epub zip file contents as a
    ByteString to the epub-metadata library. Need to read this data
    strictly to avoid dangling open files.
  * Corrected for breakage due to change in title format of Eclipse magazine
  * Some work done on the utility script for deploying Windows
    binaries of these tools
  * Added parsing support and test cases for more date formats
  * Minor usage info changes


1.1.1 (2011-11-15)

  * Changed how publication date is found to more closely follow
    the OPF spec recommendations
  * Changed the switches related to publication date
  * Redesigned unit test code and added more tests for new date code


1.1.0 (2011-11-04)

  * Huge redesign of how formatting works, dramatically shortening
    the code needed to handle any given book type. Code is much more
    monadic now and consolidated into one module.
  * Many changes/additions to magazine and compilation book name
    formatting
  * Fixed a group of bugs that occur when a creator has only a
    single word for their name
  * Extensive changes/additions to unit testing for above


1.0.0.1 (2011-10-27)

  * Extensive changes to the cabal build of this project to bring
    it up to Cabal 1.10
  * Unit tests now use the test-suite cabal stanza


1.0.0.0 (2011-04-23)

  * Initial release
