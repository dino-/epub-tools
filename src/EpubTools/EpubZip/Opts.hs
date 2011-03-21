-- Copyright: 2010, 2011 Dino Morelli
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
   }


defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
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
         [ "Usage: epubzip [OPTIONS] EPUBFILE"
         , "Construct an epub zip archive from files in the current directory"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Version 1.0.0.0  Dino Morelli <dino@ui3.info>"
         ]
