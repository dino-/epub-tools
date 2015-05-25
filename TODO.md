## general

- Update cabal tested-with
- README.md changes:
   - installing epubcheck if possible
   - less-obscure example book naming sessions
   - demonstration of interactive mode
   - encourage users to contact the developer (see below in epubname as well)
- Windows binary docs:
   - make INSTALL more nicely worded
   - add information or maybe a batch file for assisting with globs and cmd
- Use Paths_epub_tools version instead of hard-coded version in the Opts modules. Test this on Windows.
- Would like to way to be able to insert a publishing year tag into the OPF data
   - like these:
      - `<dc:date opf:event="publication">2011</dc:date>`
      - `<dc:date opf:event="original-publication">2011</dc:date>`
      - `<dc:subject>anthology</dc:subject>`
   - Perhaps as part of epubmeta?
- Perform util/win-dist.sh behavior in post-build step of Setup.hs? I think it makes sense.


## epubmeta

- When a backup is made during `-e[SUF]`, make a note of it on stdout
- Maybe more options for modifying metadata from the command-line
- Ability to dump the actual Haskell Metadata structure out. Could be used to make unit tests for epubname.


## epubname

- in usage, encourage users to contact the developer
- Add info in the usage about submitting metadata to me using epubmeta when there are problems.
- Rethink that Publisher business, maybe get rid of it.
- I'd like to see the author name formatting be more flexible so you can opt to not have it call scrubString as is mandatory now.
- Can possibly parallelize book renaming, each book is an atomic operation. Look into it.
- Not testing improper name instruction function arguments (such as: an index that doesn't exist in a match with `idx`)
   - This is tricky as we're using the stock set of rules that are deployed for the real app for all testing. Need to redesign `testsuite/EpubTools/test-epubname.main` to be able to load a different set of rules.
- Why is the verbosity Opts value a Maybe Int? Couldn't it just have been an Int (0, 1 or 2) where 0 is the old Nothing? Or even better, explicit values like: `data Verbosity = Minimum | ShowFormatter | Maximum`  Think about it more

- Try to make tests of formatters into a conf file thing as well. This is kind of important. Not part of HUnit or QC. module hierarchy:

      EpubName/
         Test/
            Compile
            Test


## epubzip
