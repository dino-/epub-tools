{- # LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module EpubTools.EpubMeta.Opts
   -- -- ( Edit (..)
   -- -- , Export (..)
   -- ( Options (..), defaultOptions
   -- , parseOpts
   -- -- , parseOpts, usageText
   -- )
   where

import Data.Version ( showVersion )
import Options.Applicative
import Paths_epub_tools ( version )
import System.Environment (getProgName)
import Text.Printf (printf)

import EpubTools.EpubMeta.Util


-- -- -- There are three states for this, representing with a new type
-- data Edit
--    = Backup String
--    | NoBackup
--    | NotEditing
--    deriving (Eq, Show)
--    -- deriving Eq

-- maybeToEdit :: Maybe String -> Edit
-- maybeToEdit (Just suffix) = Backup suffix
-- maybeToEdit Nothing       = NoBackup


-- -- There are three states for this, representing with a new type
-- data Export
--    = ToPath String   -- User specified a file to export to
--    | Existing        -- User wants export to use existing name
--    | NoExport        -- This arg wasn't used at all
--    deriving (Eq, Show)
--    -- deriving Eq


-- maybeToExport :: Maybe String -> Export
-- maybeToExport (Just file) = ToPath file
-- maybeToExport Nothing     = Existing

{-
    ( -V [-v]
    | -X [(-o FILE|-e)]
    | -E [-b SUF]
    | -I FILE [-b SUF]
    )
    FILE
-}


newtype Verbose = Verbose Bool
  deriving Show

data Output
  = OutputFilename FilePath
  | ExistingName
  | StdOut
  deriving Show

data Backup
   = BackupSuffix FilePath
   | NoBackup
   deriving Show

newtype ImportPath = ImportPath FilePath
  deriving Show

data Mode
  = View Verbose
  | Export Output
  | Edit Backup
  | Import ImportPath Backup
  deriving Show

data Options = Options
  { mode :: Mode
  , inFile :: FilePath
  }
  deriving Show

-- defaultOptions :: Options
-- defaultOptions = Options (View (Verbose False)) "unknown_file"


-- data Options = Options
--    { optEdit :: Edit
--    , optExport :: Export
--    , optImport :: Maybe String
--    , optVerbose :: Bool
--    }
--    deriving Show


-- defaultOptions :: Options
-- defaultOptions = Options
--    { optEdit = NotEditing
--    , optExport = NoExport
--    , optImport = Nothing
--    , optVerbose = False
--    }


parseView :: Parser Mode
parseView = View
  <$> ( flag' ()
        (  long "view"
        <> short 'V'
        <> help "View EPUB metadata, this is the default mode"
        )
        *>  ( Verbose <$> switch
              (  long "verbose"
              <> short 'v'
              <> help "Display all OPF package info, including manifest, spine and guide"
              )
            )
      )

parseEdit :: Parser Mode
parseEdit = Edit
  <$> ( flag' ()
        (  long "edit"
        <> short 'E'
        <> help "Edit a book's OPF XML data in a text editor"
        )
        *>  ( maybe NoBackup BackupSuffix <$> (optional $ strOption
              (  long "backup"
              <> short 'b'
              <> metavar "SUF"
              )
            ))
      )

parseExport :: Parser Mode
parseExport = Export
  <$> ( flag' ()
        (  long "export"
        <> short 'X'
        <> help "Export the book's OPF XML metadata, defaults to stdout unless -o or -e are used"
        )
        *>  ( ( OutputFilename <$> strOption
                (  long "output"
                <> short 'o'
                <> metavar "FILE"
                <> help "Filename to use for exporting the OPF XML metadata document"
                )
              )
              <|>
              ( flag' ExistingName
                (  long "existing"
                <> short 'e'
                <> help "Export OPF XML metadata document with the same filename used in the book archive"
                )
              )
              <|>
              pure StdOut
            )
      )

parseImport :: Parser Mode
parseImport = Import
  <$> ( ImportPath <$> strOption
        (  long "import"
        <> short 'I'
        <> metavar "FILE"
        <> help "Import OPF metadata from the supplied XML file"
        )
      )
  <*> ( ( maybe NoBackup BackupSuffix <$> (optional $ strOption
          (  long "backup"
          <> short 'b'
          <> metavar "SUF"
          <> help "Make a backup of the EPUB file with this suffix appended to the filename"
          )
        ))
      )

-- This defaults to `-V EPUBFILE` behavior when invoked with just EPUBFILE
parseNoMode = pure (View (Verbose False))

parseExplicitMode = parseView <|> parseEdit <|> parseExport <|> parseImport

parser :: Parser Options
parser = Options
  <$> ( parseNoMode <|> parseExplicitMode )
  <*> ( argument str
        (  metavar "EPUBFILE"
        <> help "Path to EPUB file"
        )
      )


-- data Options = Options
--   { mode :: Mode
--   , backup :: Backup
--   , verbose :: Verbose
--    -- { optEdit :: Edit
--    -- , optExport :: Export
--    -- , optImport :: Maybe String
--    -- , optVerbose :: Bool
--    }
--    deriving Show


-- defaultOptions :: Options
-- defaultOptions = Options
--    { optEdit = NotEditing
--    , optExport = NoExport
--    , optImport = Nothing
--    , optVerbose = False
--    }


-- parser :: Parser Options
-- parser = Options
--   <$> ( ( flag' NoBackup
--           (  long "edit"
--           <> short 'e'
--           <> help "Edit a book's OPF XML data in a text editor"
--           )
--         )
--         <|>
--         ( Backup <$> strOption
--           (  long "edit-with-backup"
--           <> short 'b'
--           <> metavar "SUF"
--           <> help "Edit a book's OPF XML data in a text editor. A backup of the book will be created with the supplied suffix"
--           )
--         )
--         <|>
--         pure NotEditing
--       )
--   <*> ( maybeToExport <$> (optional $ strOption
--         (  long "export"
--         <> short 'x'
--         <> metavar "FILE"
--         <> help "Export the book's OPF XML metadata to a file. If no file name is given, the name of the file in the book will be used"
--         )
--       ))
--   <*> ( optional $ strOption
--         (  long "import"
--         <> short 'i'
--         <> metavar "FILE"
--         <> help "Import OPF metadata from the supplied XML file"
--         )
--       )
--   <*> ( switch
--         (  long "verbose"
--         <> short 'v'
--         <> help "Display all OPF package info, including manifest, spine and guide"
--         )
--       )


-- parser :: Parser Options
-- parser = Options
--   -- <$> ( maybeToEdit <$> (optional $ strOption
--   --       (  long "edit-opf"
--   --       <> short 'e'
--   --       <> metavar "SUF"
--   --       <> help "Edit a book's OPF XML data in a text editor. If an optional suffix is supplied, a backup of the book will be created with that suffix"
--   --       )
--   --     ))
--   <$> ( ( Backup <$> strOption
--           (  long "edit-opf"
--           <> short 'e'
--           <> metavar "SUF"
--           <> help "Edit a book's OPF XML data in a text editor. A backup of the book will be created with the supplied suffix"
--           )
--         )
--         <|>
--         ( flag' NoBackup
--           (  long "edit-opf"
--           <> short 'e'
--           <> help "Edit a book's OPF XML data in a text editor"
--           )
--         )
--         <|>
--         pure NotEditing
--       )
--   -- <$> ( ( flag NotEditing NoBackup
--   --         (  long "edit-opf"
--   --         <> short 'e'
--   --         <> help "Edit a book's OPF XML data in a text editor"
--   --         )
--   --       )
--   --       <|>
--   --       ( maybeToEdit <$> (optional $ strOption
--   --         (  long "edit-opf"
--   --         <> short 'e'
--   --         <> metavar "SUF"
--   --         <> help "Edit a book's OPF XML data in a text editor. A backup of the book will be created with the supplied suffix"
--   --         )
--   --       ))
--   --     )
--   <*> ( maybeToExport <$> (optional $ strOption
--         (  long "export"
--         <> short 'x'
--         <> metavar "FILE"
--         <> help "Export the book's OPF XML metadata to a file. If no file name is given, the name of the file in the book will be used"
--         )
--       ))
--   <*> ( optional $ strOption
--         (  long "import"
--         <> short 'i'
--         <> metavar "FILE"
--         <> help "Import OPF metadata from the supplied XML file"
--         )
--       )
--   <*> ( switch
--         (  long "verbose"
--         <> short 'v'
--         <> help "Display all OPF package info, including manifest, spine and guide"
--         )
--       )


-- parseOpts :: IO Options
parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  -- execParser $ info (parser <**> helper <**> parseVersion pn)
  execParser $ info (parser <**> helper)
    (  header (printf "%s - View or edit epub book metadata" pn)
    -- <> footer'
    )


-- options :: [OptDescr (Options -> Options)]
-- options =
--    [ Option ['h'] ["help"] 
--       (NoArg (\opts -> opts { optHelp = True } ))
--       "This help text"
--    , Option ['e'] ["edit-opf"]
--       (OptArg (\f opts -> opts { optEdit = maybeToEdit f }) "SUF")
--       "Edit a book's OPF XML data in a text editor. If an optional suffix is supplied, a backup of the book will be created with that suffix"
--    , Option ['x'] ["export"]
--       (OptArg (\f opts -> opts { optExport = maybeToExport f }) "FILE")
--       "Export the book's OPF XML metadata to a file. If no file name is given, the name of the file in the book will be used"
--    , Option ['i'] ["import"]
--       (ReqArg (\f opts -> opts { optImport = Just f }) "FILE")
--       "Import OPF metadata from the supplied XML file"
--    , Option ['v'] ["verbose"]
--       (NoArg (\opts -> opts { optVerbose = True } )) 
--       "Display all OPF package info, including manifest, spine and guide"
--    ]


-- parseOpts :: [String] -> EM (Options, [String])
-- parseOpts argv = 
--    case getOpt Permute options argv of
--       (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
--       (_,_,errs) -> throwError $ concat errs ++ usageText


-- usageText :: String
-- usageText = (usageInfo header options) ++ "\n" ++ footer
--    where
--       header = init $ unlines
--          [ "Usage: epubmeta [OPTIONS] EPUBFILE"
--          , "View or edit epub book metadata"
--          , ""
--          , "Note that 'edit' here means you get dumped into an editor with the XML document and are on your own."
--          , ""
--          , "Options:"
--          ]
--       footer = init $ unlines
--          [ "The -e feature will look for an editor in this order: the EDITOR environment variable, the VISUAL environment variable, vi"
--          , ""
--          , "For more information on the epub format:"
--          , "   epub2: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm"
--          , "   epub3: http://www.idpf.org/epub/30/spec/epub30-publications.html"
--          , ""
--          , ""
--          , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
--          ]
