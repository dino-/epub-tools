-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module EpubTools.EpubName.Opts
   ( Options (..)
   , defaultOptions
   , parseOpts, usageText
   )
   where

import Data.Maybe
import System.Console.GetOpt
import System.Exit


data Options = Options
   { optHelp :: Bool
   , optNoAction :: Bool
   , optOverwrite :: Bool
   , optPublisher :: Bool
   , optVerbose :: Maybe Int
   , optYear :: Bool
   }


defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   , optNoAction = False
   , optOverwrite = False
   , optPublisher = False
   , optVerbose = Nothing
   , optYear = True
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['n'] ["no-action"]
      (NoArg (\opts -> opts { optNoAction = True } )) 
      "Display what would be done, but do nothing"
   , Option ['o'] ["overwrite"]
      (NoArg (\opts -> opts { optOverwrite = True } )) 
      "Overwrite existing file with new name"
   , Option ['p'] ["publisher"]
      (NoArg (\opts -> opts { optPublisher = True } )) 
      "Include book publisher if present. See below"
   , Option ['v'] ["verbose"]
      ( OptArg
         ((\n opts -> opts { optVerbose = Just (read n)}) . fromMaybe "1")
         "LEVEL")
      "Verbosity level: 1, 2"
   , Option ['y'] ["year"]
      (NoArg (\opts -> opts { optPublisher = True } )) 
      "Suppress inclusion of original publication year, if present"
   ]


parseOpts :: [String] -> IO (Either ExitCode (Options, [String]))
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return . Right $ (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> do
         putStrLn $ concat errs ++ usageText
         return . Left $ ExitFailure 1


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
         , "   1      - Include which formatter processed the file"
         , "   2      - Include the OPF Package and Metadata info"
         , ""
         , "Exit codes:"
         , "   0 - success"
         , "   1 - bad options"
         , "   2 - failed to process one or more of the files given"
         , ""
         , "Book names are constructed by examining parts of the OPF Package metadata such as the title, creators, contributors and dates."
         , ""
         , "For books with a single author:"
         , "   LastFirst[Middle]-TitleText[_year][_publisher].epub"
         , ""
         , "For books with multiple authors:"
         , "   Last1_Last2[_Last3...]-TitleText[_year][_publisher].epub"
         , ""
         , "For books that have no clear authors, such as compilations:"
         , "   TitleText[_year][_publisher].epub"
         , ""
         , "Only creator tags with either a role attribute of 'aut' or no role at all are considered authors. If a file-as attribute is present, this will be the preferred string. If not, the program tries to do some intelligent parsing of the name."
         , ""
         , "The date is taken from the first date tag with an event attribute of 'original-publication', if present. The OPF spec is a little thin on this. I noticed frequent use of this attribute in books, and so went with that."
         , ""
         , "Publisher: I wanted to provide a way to have multiple copies of the same book produced by different publishers and name them sort-of unambiguously. I came up with the idea of expecting a contributor tag with role attribute of 'bkp' (so far, this is fairly normal). And then use a file-as attribute on that tag to contain a small string to be used in the filename. The idea here is short and sweet for the file-as."
         , ""
         , "Magazines are kind of a sticky problem in that it's often desireable to have edition and/or date info in the filename. There's a lot of chaos out there with titling the epub editions of magazines. The solution in this software is to do some pattern matching on multiple fields in the magazine's metadata combined with custom naming code for specific magazines. This means that support for future magazines will likely have to be hand-coded into future versions of this utility. Modifying this just isn't very non-programmer friendly."
         , ""
         , "Version 1.0.0.0  Dino Morelli <dino@ui3.info>"
         ]
