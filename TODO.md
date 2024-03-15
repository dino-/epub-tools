## general

- README.md changes:
   - installing epubcheck if possible
   - less-obscure example book naming sessions
   - demonstration of interactive mode
   - encourage users to contact the developer (see below in epubname as well)
- Windows binary docs:
   - make INSTALL more nicely worded
   - add information or maybe a batch file for assisting with globs and cmd
- Would like to way to be able to insert a publishing year tag into the OPF data
   - like these:
      - `<dc:date opf:event="publication">2011</dc:date>`
      - `<dc:date opf:event="original-publication">2011</dc:date>`
      - `<dc:subject>anthology</dc:subject>`
   - Perhaps as part of epubmeta?


## epubmeta

- When a backup is made during `-b|--backup SUF`, make a note of it on stdout
- Maybe more options for modifying metadata from the command-line
- Ability to dump the actual Haskell Metadata structure out. Could be used to make unit tests for epubname. Would want to use a pretty printer library for this as opposed to just the Show instancing.


## epubname

- Switch from using String everywhere to Text. This may alleviate some of the special typographic character problems I have from time to time.
- In usage, encourage users to contact the developer
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

2024-02-28

- Redesign epubname.hs, Main.hs. These modules are poorly named and too much weird stuff is lumped together in this area of the code.
- Change one or two date examples in the usage to just YYYY
- When does exitProcessingFailure actually happen? Seems like never. Examine what processBook does more closely.


## epubzip
