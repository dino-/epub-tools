-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubZip.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import System.Console.GetOpt


data Options = Options
   { optHelp :: Bool
   , optOverwrite :: Bool
   }


defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   , optOverwrite = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['o'] ["overwrite"] 
      (NoArg (\opts -> opts { optOverwrite = True } ))
      "Force overwrite if dest file exists"
   ]


parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: epubzip [OPTIONS] DESTDIR"
         , "       epubzip [OPTIONS] DESTDIR/EPUBFILE"
         , "Construct an EPUB2 zip archive from files in the current directory"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "If run with DESTDIR alone, epubzip will try to construct a name from the OPF package data for this book (see epubname). If run with DESTDIR/EPUBFILE, epubzip will use that name for the destination file."
         , ""
         , "You may have noticed that there is no epubunzip utility. Truth is, epubs are just zip files and you barely need epubzip either if you have the normal zip/unzip utilities installed. While not as fancy with file naming and leaving out dotfiles, this works for zipping:"
         , ""
         , "   $ cd DIR"
         , "   $ zip -Xr9D ../EPUBFILE mimetype *"
         , ""
         , "And for unzipping, it's really just as easy:"
         , ""
         , "   $ mkdir TEMPDIR"
         , "   $ cd TEMPDIR"
         , "   $ unzip EPUBFILE"
         , ""
         , "For more information please see the IDPF OPF specification found here: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm"
         , ""
         , "Version 2.3  Dino Morelli <dino@ui3.info>"
         ]
