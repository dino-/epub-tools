-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Opts
   ( Edit (..)
   , Export (..)
   , Options (..), defaultOptions
   , parseOpts, usageText
   )
   where

import Data.Version ( showVersion )
import Paths_epub_tools ( version )
import System.Console.GetOpt

import EpubTools.EpubMeta.Util


-- There are three states for this, representing with a new type
data Edit
   = Backup String
   | NoBackup
   | NotEditing
   deriving Eq


maybeToEdit :: Maybe String -> Edit
maybeToEdit (Just suffix) = Backup suffix
maybeToEdit Nothing       = NoBackup


-- There are three states for this, representing with a new type
data Export
   = ToPath String   -- User specified a file to export to
   | Existing        -- User wants export to use existing name
   | NoExport        -- This arg wasn't used at all
   deriving Eq


maybeToExport :: Maybe String -> Export
maybeToExport (Just file) = ToPath file
maybeToExport Nothing     = Existing


data Options = Options
   { optHelp :: Bool
   , optEdit :: Edit
   , optImport :: Maybe String
   , optExport :: Export
   , optVerbose :: Bool
   }


defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   , optEdit = NotEditing
   , optImport = Nothing
   , optExport = NoExport
   , optVerbose = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['e'] ["edit-opf"]
      (OptArg (\f opts -> opts { optEdit = maybeToEdit f }) "SUF")
      "Edit a book's OPF XML data in a text editor. If an optional suffix is supplied, a backup of the book will be created with that suffix"
   , Option ['x'] ["export"]
      (OptArg (\f opts -> opts { optExport = maybeToExport f }) "FILE")
      "Export the book's OPF XML metadata to a file. If no file name is given, the name of the file in the book will be used"
   , Option ['i'] ["import"]
      (ReqArg (\f opts -> opts { optImport = Just f }) "FILE")
      "Import OPF metadata from the supplied XML file"
   , Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True } )) 
      "Display all OPF package info, including manifest, spine and guide"
   ]


parseOpts :: [String] -> EM (Options, [String])
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> throwError $ concat errs ++ usageText


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: epubmeta [OPTIONS] EPUBFILE"
         , "View or edit epub book metadata"
         , ""
         , "Note that 'edit' here means you get dumped into an editor with the XML document and are on your own."
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "The -e feature will look for an editor in this order: the EDITOR environment variable, the VISUAL environment variable, vi"
         , ""
         , "For more information on the epub format:"
         , "   epub2: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm"
         , "   epub3: http://www.idpf.org/epub/30/spec/epub30-publications.html"
         , ""
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
