## general

This project may benefit from the use of some other tools out here
such as epubcheck, zip and unzip

- Tools unzipping can try to fall back on unzip upon failure
- epubmeta needs write capability
- epub creation, think about this
- always release Windows binaries
- OSX too?


## 2015-05-17 redesigning tryFormatting and the EN monad

Running into a problem where we don't necessarily want all failures of any kind to be expressed as `MonadError String` any longer. There are at least two types of failure along the way here:

1. a match failure of a regexp, which we don't want details on
2. a failure to parse or execute a naming DSL instruction, we do want this reported

`EpubTools.EpubName.Format.Format.tryFormatting` looks like this:

      {- Try the entire list of formatters, one by one, in order, until one
         succeeds or none do
      -}
      tryFormatting :: (MonadIO m, MonadError FormatError m) =>
         Globals -> [Formatter] -> FilePath -> m (String, FilePath)
      tryFormatting gs fs oldPath =
         either throwError return $ runEN gs $
            foldr mplus
               (throwError $ FinalError $ printf "%s [ERROR No formatter found]" oldPath) $
               map tryFormatter fs


## Found some old notesbook notes about the DSL:

I think this was maybe about a less-flat more language-like DSL with recursive interpretation. But this is not how the software runs today. Maybe it should in the future?

    aut  (aut)
    idx  (idx 2)
    lit  some string
    mnum (mnum (idx 2))
    pad  (pad 2 (idx 1))
    publ (publ)
    scr  (scr (idx 3))
    wnum (wnum 3 (idx 4))
    year (year)
