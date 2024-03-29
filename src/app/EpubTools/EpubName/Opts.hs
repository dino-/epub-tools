{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module EpubTools.EpubName.Opts
  ( parseOpts
  )
  where

import Data.List.NonEmpty ((<|), fromList, singleton)
import Data.Version (showVersion)
import Options.Applicative
import Paths_epub_tools (version)
import System.Environment (getProgName)
import Text.Heredoc (here)
import Text.PrettyPrint.ANSI.Leijen (string)
import Text.Printf (printf)

import EpubTools.EpubName.Common
  ( BookFiles (..)
  , InteractiveSwitch (..)
  , MoveSwitch (..)
  , NoActionSwitch (..)
  , Options (Options)
  , PublisherSwitch (..)
  , PubYear (AnyDate, NoDate, NoModified)
  , RulesLocation (RulesPath), RulesLocations (..)
  , TargetDirs (..)
  , defaultRulesLocations
  , intToVerbosity
  )
import qualified EpubTools.EpubName.Doc.Dsl as Dsl
import qualified EpubTools.EpubName.Doc.Rules as Rules


prependRulesLocation :: RulesLocations -> Maybe FilePath -> RulesLocations
prependRulesLocation rulesLocations Nothing = rulesLocations
prependRulesLocation (RulesLocations rulesLocList) (Just filePath) =
  RulesLocations ((RulesPath filePath) <| rulesLocList)


setTargetDirs :: [FilePath] -> TargetDirs
setTargetDirs [] = TargetDirs $ singleton "."
setTargetDirs ds = TargetDirs $ fromList ds


parser :: Parser Options
parser = Options
  <$> ( ( flag AnyDate NoModified
          (  long "no-modified-date"
          <> short 'M'
          <> help "Don't use modified date for publication year"
          )
        )
        <|>
        ( flag AnyDate NoDate
          (  long "no-date"
          <> short 'D'
          <> help "Suppress inclusion of any publication year"
          )
        )
      )
  <*> ( InteractiveSwitch <$> switch
        (  long "interactive"
        <> short 'i'
        <> help "Prompt user interactively"
        )
      )
  <*> ( NoActionSwitch <$> switch
        (  long "no-action"
        <> short 'n'
        <> help "Display what would be done, but do nothing"
        )
      )
  <*> ( PublisherSwitch <$> switch
        (  long "publisher"
        <> short 'p'
        <> help "Include book publisher if present. See below"
        )
      )
  <*> ( prependRulesLocation defaultRulesLocations <$> (optional $ strOption
        (  long "rules"
        <> short 'r'
        <> metavar "FILE"
        <> help "Specify a rules file for naming the books"
        )
      ))
  <*> ( setTargetDirs <$> (many $ strOption
        (  long "target-dir"
        <> short 't'
        <> metavar "DIR"
        <> help "Target directory for successfully renamed books. Specify multiple times for additional directories (default: \".\")"
        )
      ))
  <*> ( MoveSwitch <$> switch
        (  long "move"
        <> help "Remove the original file after linking (or copying) to target directories"
        )
      )
  <*> ( intToVerbosity <$> option auto
        (  long "verbose"
        <> short 'v'
        <> metavar "NUM"
        <> help "Verbosity level: 0, 1 or 2, see VERBOSITY LEVELS below"
        <> showDefault
        <> value 0
        )
      )
  <*> ( BookFiles . fromList <$> (some $ strArgument
        (  metavar "FILES"
        <> help "One or more files to rename/move"
        )
      ))


parseDumpRules :: Parser (a -> a)
parseDumpRules =
  infoOption Rules.defaults $ mconcat
  [ long "dump-rules"
  , help "Output built-in rules definitions, use to make your own"
  ]


parseHelpRules :: Parser (a -> a)
parseHelpRules =
  infoOption Dsl.docs $ mconcat
  [ long "help-rules"
  , help "Help on the naming rules domain specific language"
  ]


parseVersion :: String -> Parser (a -> a)
parseVersion progName =
  infoOption (printf "%s %s" progName (showVersion version)) $ mconcat
  [ long "version"
  , help "Show version information"
  ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> parseDumpRules <**> parseHelpRules <**> parseVersion pn)
    (  header (printf "%s - Rename epub book files with clear names based on their metadata" pn)
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . string $ (printf content (showVersion version) :: String)
  where content = [here|VERBOSITY LEVELS:

  0 - Normal verbosity
  1 - Output which formatter processed the file
  2 - Output the OPF Package and Metadata info

EXIT CODES:

  0 - success
  1 - bad options
  2 - failed to process one or more of the files given

SIMPLE NAMING:

Book names are constructed by examining parts of the OPF Package metadata such as the title, creators, contributors and dates.

Strings from the OPF metadata fields are stripped of punctuation, CamelCased and stripped of spaces. Resulting file names look like this:

For books with a single author:
  LastFirst[Middle]-TitleText[_year][_publisher].epub

For books with multiple authors:
  Last1_Last2[_Last3...]-TitleText[_year][_publisher].epub

For books that have no clear authors, such as compilations:
  TitleText[_year][_publisher].epub

Only creator tags with either a role attribute of 'aut' or no role at all are considered authors. If a file-as attribute is present, this will be the preferred string. If not, the program tries to do some intelligent parsing of the name.

PUBLICATION DATE:

This software will attempt to locate a publication date to use at the end of the filename. This can be suppressed with the --no-date switch. How the date is found depends on the document epub version:

epub2: These date values are looked for in this order
  issued    <dc:date opf:event='original-publication'>2013-09-17</dc:date>
  created   <dc:date opf:event='publication'>2013-09-17T12:00:00Z</dc:date>
  epub      <dc:date>2013-09-17</dc:date>
  modified  <meta property='dcterms:modified'>2013-09-17</meta>

epub3: These date values are looked for in this order
  issued    <meta property='dcterms:issued'>2013-09-17</meta>
  created   <meta property='dcterms:created'>2013-09-17T12:00:00Z</created>
  date      <meta property='dcterms:date'>2013-09-18</meta>
  epub      <dc:date>2013-09-17T12:00:00Z</dc:date>
  modified  <meta property='dcterms:modified'>2013-09-17</meta>

This software will fish the year out of whatever string it can find. Use the examples above as a guide for adding/editing these values yourself.

PUBLISHER:

I wanted to provide a way to have multiple copies of the same book produced by different publishers and name them sort-of unambiguously. I came up with the idea of expecting a contributor tag with role attribute of 'bkp' (so far, this is fairly normal). And then use a file-as attribute on that tag to contain a small string to be used in the filename. The idea here is short and sweet for the file-as.

MORE COMPLICATED NAMING: MAGAZINES AND COLLECTIONS

Magazines are kind of a sticky problem in that it's often desireable to have edition and/or date info in the filename. There's a lot of chaos out there with titling the epub editions of magazines. The solution in this software is to do some pattern matching on multiple fields in the magazine's metadata combined with custom naming rules for specific magazines.

This software ships with a set of rules to properly name over 20 magazines and compilations that are commonly purchased by the developer. These default rules can be extended to add new publications, and used as an example.

Another case is anthologies, which is to say: collections of works by multiple authors but not necessarily in periodical form. There is an anthology rule in the default rules that's set to look for any <dc:subject>anthology</dc:subject> tag in the metadata. In this case, the authors will be ignored and the title along with a publication year will be used (see above re: dc:date)

epubname will search the following locations for a rules file, in this order:

  1. Path specified in the optional --rules switch
  2. $HOME/.config/epubtools/default.rules   Modern UNIX-like location
  3. $HOME/.epubtools/default.rules          Older UNIX-like location
  4. %%APPDATA%%\\epubtools\\default.rules   Windows
  5. Built-in rules, a comprehensive stock set of naming rules

Use the built-in rules as a model for a custom file. See --dump-rules above

Please see --help-rules for more information on the syntax of the DSL in the rules file.


For more information on the epub format:
  epub2: http://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm
  epub3: https://www.w3.org/TR/epub/


Version %s  Dino Morelli <dino@ui3.info>"|]
