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
         , "Book names are constructed by examining creator tags, the title tag, and one of the date tags in the OPF Package metadata."
         , ""
         , "For books with a single author:"
         , "   LastFirst[Middle]-TitleText[_Year].epub"
         , ""
         , "For books with multiple authors:"
         , "   Last1_Last2[_Last3...]-TitleText[_Year].epub"
         , ""
         , "For books that have no clear authors, such as compilations which are typically not filed by author:"
         , "   TitleText[_Year].epub"
         , ""
         , "Only creator tags with either a role attribute of 'aut' or no role at all are considered authors. If a file-as attribute is present, this will be the preferred string. If not, the program tries to do some intelligent parsing of the name."
         , ""
         , "The date is taken from date tags with an event attribute of 'original-publication' The OPF spec is a little thin on this. Going by what I've seen in the vast majority of epub documents."
         , ""
         , "Magazines are kind of a sticky problem in that it's often desireable to have edition and/or date info in the filename. There's a lot of chaos out there with titling the epub editions of magazines. The solution in this software is to do some pattern matching on multiple fields in the magazine's metadata combined with custom naming code for specific magazines. This means that support for future magazines will likely have to be hand-coded into future versions of this utility."
         , ""
         , "Version 2.0.1  Dino Morelli <dino@ui3.info>"
         ]
