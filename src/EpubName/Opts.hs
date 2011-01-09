-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module EpubName.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Data.Maybe
import System.Console.GetOpt


data Options = Options
   { optHelp :: Bool
   , optNoAction :: Bool
   , optVerbose :: Maybe Int
   }


defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   , optNoAction = False
   , optVerbose = Nothing
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['n'] ["no-action"]
      (NoArg (\opts -> opts { optNoAction = True } )) 
      "Display what would be done, but do nothing"
   , Option ['v'] ["verbose"]
      ( OptArg
         ((\n opts -> opts { optVerbose = Just (read n)}) . fromMaybe "1")
         "LEVEL")
      "Verbosity level: 1, 2"
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
         [ "Usage: epubname [OPTIONS] FILES"
         , "Rename EPUB book files with clear names based on their metadata"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Verbosity levels:"
         , "   <none> - Just display the oldname -> newname output"
         , "   1      - Include which formatter processed the file"
         , "   2      - Include the OPF Package and Metadata info"
         , ""
         , "Version 2.0.0  Dino Morelli <dino@ui3.info>"
         ]
