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
import Text.Heredoc (here)
import Text.PrettyPrint.ANSI.Leijen (string)
import Text.Printf (printf)


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
parseNoMode :: Parser Mode
parseNoMode = pure (View (Verbose False))

parseExplicitMode :: Parser Mode
parseExplicitMode = parseView <|> parseEdit <|> parseExport <|> parseImport

parser :: Parser Options
parser = Options
  <$> ( parseNoMode <|> parseExplicitMode )
  <*> ( argument str
        (  metavar "EPUBFILE"
        <> help "Path to EPUB file"
        )
      )


parseVersion :: String -> Parser (a -> a)
parseVersion progName =
  infoOption (printf "%s %s" progName (showVersion version)) $ mconcat
  [ long "version"
  , help "Show version information"
  ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> parseVersion pn)
    (  header (printf "%s - View or edit epub book metadata" pn)
    <> footer'
    )

footer' :: InfoMod a
footer' = footerDoc . Just . string $ (printf content (showVersion version) :: String)
  where content = [here|-E|--edit mode will look for an editor in this order: the EDITOR environment variable, the VISUAL environment variable, vi
Note that edit mode means you get dumped into the editor with the XML document and are on your own.

For more information on the epub format:
  epub2: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm
  epub3: http://www.idpf.org/epub/30/spec/epub30-publications.html


Version %s  Dino Morelli <dino@ui3.info>|]
