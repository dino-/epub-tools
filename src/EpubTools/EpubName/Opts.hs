-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module EpubTools.EpubName.Opts
   ( PubYear (..), Options (..)
   , defaultOptions
   , parseOpts, usageText
   )
   where

import Data.Maybe
import System.Console.GetOpt
import System.Exit


data PubYear
   = Publication  -- Default, use date event='publication' or 'original-publication'
   | AnyDate      -- Use first date element found
   | NoDate       -- Don't do publication date at all


data Options = Options
   { optHelp :: Bool
   , optNoAction :: Bool
   , optOverwrite :: Bool
   , optPublisher :: Bool
   , optVerbose :: Maybe Int
   , optPubYear :: PubYear
   }


defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   , optNoAction = False
   , optOverwrite = False
   , optPublisher = False
   , optVerbose = Nothing
   , optPubYear = Publication
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['d'] ["any-date"]
      (NoArg (\opts -> opts { optPubYear = AnyDate } )) 
      "If no publication year found, use first date element of any kind"
   , Option ['D'] ["no-date"]
      (NoArg (\opts -> opts { optPubYear = NoDate } )) 
      "Suppress inclusion of original publication year"
   , Option ['h'] ["help"] 
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
         , "   1 - Include which formatter processed the file"
         , "   2 - Include the OPF Package and Metadata info"
         , ""
         , "Exit codes:"
         , "   0 - success"
         , "   1 - bad options"
         , "   2 - failed to process one or more of the files given"
         , ""
         , "Book names are constructed by examining parts of the OPF Package metadata such as the title, creators, contributors and dates."
         , ""
         , "Strings from the OPF metadata fields are stripped of punctuation, CamelCased and stripped of spaces. Resulting file names look like this:"
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
         , "The OPF spec suggests there may be a <dc:date opf:event='publication'>2011</dc:date> element representing original publication date. If this (or opf:event='original-publication') is present, it will be used by default for _year as in the above examples. The --any-date switch will use the first date tag found, regardless of attributes. The year can be parsed out of many date formats, it's very flexible."
         , ""
         , "Publisher: I wanted to provide a way to have multiple copies of the same book produced by different publishers and name them sort-of unambiguously. I came up with the idea of expecting a contributor tag with role attribute of 'bkp' (so far, this is fairly normal). And then use a file-as attribute on that tag to contain a small string to be used in the filename. The idea here is short and sweet for the file-as."
         , ""
         , "Magazines are kind of a sticky problem in that it's often desireable to have edition and/or date info in the filename. There's a lot of chaos out there with titling the epub editions of magazines. The solution in this software is to do some pattern matching on multiple fields in the magazine's metadata combined with custom naming code for specific magazines. This means that support for future magazines will likely have to be hand-coded into future versions of this utility. Modifying this just isn't very non-programmer friendly."
         , ""
         , "For more information please see the IDPF OPF specification found here: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm"
         , ""
         , "Version 2.0.0  Dino Morelli <dino@ui3.info>"
         ]
