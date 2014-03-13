-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubTools.EpubName.Opts
   ( PubYear (..), Options (..)
   , defaultOptions
   , parseOpts, usageText
   )
   where

import Control.Monad.Error
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath

import EpubTools.EpubName.Util


data PubYear
   = Publication  -- Default
      -- epub3: use first date of any kind
      -- epub2: use date 'original-publication' or event='publication'
   | AnyDate      -- Failing above, use any date found
   | NoDate       -- Don't do publication date at all


data Options = Options
   { optDumpRules    :: Bool
   , optHelp         :: Bool
   , optHelpRules    :: Bool
   , optInteractive  :: Bool
   , optNoAction     :: Bool
   , optPublisher    :: Bool
   , optRulesPaths   :: [FilePath]
   , optTargetDir    :: FilePath
   , optVerbose      :: Maybe Int
   , optPubYear      :: PubYear
   }


defaultRulesFile :: FilePath
defaultRulesFile = "default.rules"

userRulesPath :: IO FilePath
userRulesPath = do
   homeDir <- getEnv"HOME"
   return $ homeDir </> ".epubtools" </> defaultRulesFile


defaultOptions :: IO Options
defaultOptions = do
   urp <- userRulesPath

   return Options
      { optDumpRules       = False
      , optHelp            = False
      , optHelpRules       = False
      , optInteractive     = False
      , optNoAction        = False
      , optPublisher       = False
      , optRulesPaths      = [ urp ]
      , optTargetDir       = "."
      , optVerbose         = Nothing
      , optPubYear         = Publication
      }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['d'] ["any-date"]
      (NoArg (\opts -> opts { optPubYear = AnyDate } )) 
      "If no publication year found, use first date element of any kind"
   , Option ['D'] ["no-date"]
      (NoArg (\opts -> opts { optPubYear = NoDate } )) 
      "Suppress inclusion of original publication year"
   , Option [] ["dump-rules"] 
      (NoArg (\opts -> opts { optDumpRules = True } ))
      "Output built-in rules definitions, use to make your own"
   , Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option [] ["help-rules"] 
      (NoArg (\opts -> opts { optHelpRules = True } ))
      "Help on the naming rules domain specific language"
   , Option ['i'] ["interactive"] 
      (NoArg (\opts -> opts { optInteractive = True } ))
      "Prompt user interactively"
   , Option ['n'] ["no-action"]
      (NoArg (\opts -> opts { optNoAction = True } )) 
      "Display what would be done, but do nothing"
   , Option ['p'] ["publisher"]
      (NoArg (\opts -> opts { optPublisher = True } )) 
      "Include book publisher if present. See below"
   , Option ['r'] ["rules"] (ReqArg prependRulesPath "FILE")
      "Specify a rules file for naming the books"
   , Option ['t'] ["target-dir"]
      ( ReqArg
         (\p opts -> opts { optTargetDir = p } )
         "DIR")
      "Target directory for successfully renamed books. Default: ."
   , Option ['v'] ["verbose"]
      ( OptArg
         ((\n opts -> opts { optVerbose = Just (read n)}) . fromMaybe "1")
         "LEVEL")
      "Verbosity level: 1, 2"
   ]


prependRulesPath :: FilePath -> Options -> Options
prependRulesPath p opts = opts { optRulesPaths = newlist }
   where
      newlist = p : optRulesPaths opts


parseOpts :: (MonadError ExitCode m, MonadIO m) =>
   [String] -> m (Options, [String])
parseOpts argv = do
   dos <- liftIO defaultOptions
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) dos o, n)
      (_,_,errs) -> do
         liftIO $ do
            ut <- usageText
            putStrLn $ concat errs ++ ut
         throwError exitInitFailure


usageText :: IO String
usageText = return $ (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "USAGE: epubname [OPTIONS] FILES"
         , "Rename epub book files with clear names based on their metadata"
         , ""
         , "OPTIONS:"
         , ""
         ]
      footer = init $ unlines
         [ "VERBOSITY LEVELS:"
         , ""
         , "   1 - Output which formatter processed the file"
         , "   2 - Output the OPF Package and Metadata info"
         , ""
         , "EXIT CODES:"
         , ""
         , "   0 - success"
         , "   1 - bad options"
         , "   2 - failed to process one or more of the files given"
         , ""
         , "SIMPLE NAMING:"
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
         , "PUBLICATION DATE:"
         , ""
         , "This software will attempt to locate a publication date to use at the end of the filename. This can be suppressed with the --no-date switch. How the date is found depends on the document epub version:"
         , ""
         , "epub2: metadata/date elements like the examples below will be looked for in this order:"
         , "  <dc:date opf:event='original-publication'>2013-09-17</dc:date>"
         , "  <dc:date opf:event='publication'>2013-09-17T12:00:00Z</dc:date>"
         , ""
         , "The --any-date switch will force the use of the first date found in epub2 documents."
         , ""
         , "epub3: the single allowed (but optional) metadata/date element will be used and would look like this:"
         , "    <dc:date>2013-09-17T12:00:00Z</dc:date>"
         , ""
         , "My understanding is it doesn't have to be a complete date string, this software will fish the year out of whatever it can find."
         , ""
         , "PUBLISHER:"
         , ""
         , "I wanted to provide a way to have multiple copies of the same book produced by different publishers and name them sort-of unambiguously. I came up with the idea of expecting a contributor tag with role attribute of 'bkp' (so far, this is fairly normal). And then use a file-as attribute on that tag to contain a small string to be used in the filename. The idea here is short and sweet for the file-as."
         , ""
         , "MORE COMPLICATED NAMING: MAGAZINES AND COLLECTIONS"
         , ""
         , "Magazines are kind of a sticky problem in that it's often desireable to have edition and/or date info in the filename. There's a lot of chaos out there with titling the epub editions of magazines. The solution in this software is to do some pattern matching on multiple fields in the magazine's metadata combined with custom naming rules for specific magazines."
         , ""
         , "This software ships with a set of rules to properly name over 20 magazines and compilations that are commonly purchased by the developer. These default rules can be extended to add new publications, and used as an example."
         , ""
         , "Another case is anthologies, which is to say: collections of works by multiple authors but not necessarily in periodical form. There is an anthology rule in the default rules that's set to look for any <dc:subject>anthology</dc:subject> tag in the metadata. In this case, the authors will be ignored and the title along with a publication year will be used (see above re: dc:date)"
         , ""
         , "epubname will search the following locations for a rules file, in this order:"
         , ""
         , "   Path specified in a --rules switch"
         , "   $HOME/.epubtools/default.rules"
         , "   Built-in rules, a comprehensive stock set of naming rules"
         , ""
         , "Use the built-in rules as a model for a custom file. See --dump-rules above"
         , ""
         , "Please see --help-rules for more information on the syntax of the DSL in the rules file."
         , ""
         , ""
         , "For more information on the epub format:"
         , "   epub2: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm"
         , "   epub3: http://www.idpf.org/epub/30/spec/epub30-publications.html"
         , ""
         , ""
         , "Version 2.4  Dino Morelli <dino@ui3.info>"
         ]
