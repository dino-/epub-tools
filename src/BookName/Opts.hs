-- Copyright: 2007, 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module BookName.Opts
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
         [ "Usage: bookname [OPTIONS] FILES"
         , "Rename book files based on metadata"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "foo bar baz"
         , ""
         , "Version 2.0.0  Dino Morelli <dino@ui3.info>"
         ]
