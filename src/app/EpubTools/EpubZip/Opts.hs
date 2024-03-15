{-# LANGUAGE QuasiQuotes #-}

module EpubTools.EpubZip.Opts
  ( DirOrName (..)
  , Options (..)
  , Overwrite (..)
  , parseOpts
  )
  where

import Data.Version (showVersion)
import Options.Applicative
import Paths_epub_tools (version)
import System.Environment (getProgName)
import Text.Heredoc (here)
import Text.PrettyPrint.ANSI.Leijen (string)
import Text.Printf (printf)


newtype Overwrite = Overwrite Bool

newtype DirOrName = DirOrName FilePath

data Options = Options
  { overwrite :: Overwrite
  , dirOrName :: DirOrName
  }


parser :: Parser Options
parser = Options
  <$> ( Overwrite <$> switch
        (  long "overwrite"
        <> short 'o'
        <> help "Force overwrite if dest file exists"
        )
      )
  <*> ( DirOrName <$> argument str
        (  metavar "(DIR | DIR/FILE)"
        <> help "Directory or filename for the new EPUB file"
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
    (  header (printf "%s - Construct an EPUB zip archive from files in the current directory" pn)
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . string $ (printf content (showVersion version) :: String)
  where content = [here|Construct an EPUB zip archive from files in the current directory

If run with DIR alone, epubzip will try to construct a name from the OPF package data for this book (see epubname). If run with DIR/FILE, epubzip will use that name for the destination file.

You may have noticed that there is no epubunzip utility. Truth is, EPUBs are just zip files and you barely need epubzip either if you have the normal zip/unzip utilities installed. While not as fancy with file naming and leaving out dotfiles, this works for zipping:

    $ cd DIR
    $ zip -Xr9D ../EPUBFILE mimetype *

And for unzipping, it's really just as easy:

    $ mkdir TEMPDIR
    $ cd TEMPDIR
    $ unzip EPUBFILE

For more information on the EPUB format:
  EPUB2: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm
  EPUB3: http://www.idpf.org/epub/30/spec/epub30-publications.html


Version %s  Dino Morelli <dino@ui3.info>|]
