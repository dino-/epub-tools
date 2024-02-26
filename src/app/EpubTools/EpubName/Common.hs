{-# LANGUAGE DuplicateRecordFields #-}

module EpubTools.EpubName.Common
  ( BookFiles (..), DumpRulesSwitch (..), HelpRulesSwitch (..)
  , InteractiveSwitch (..), MoveSwitch (..), NoActionSwitch (..), Options (..)
  , PublisherSwitch (..), PubYear (..), RulesLocation (..), RulesLocations (..)
  , VerbosityLevel (..)
  , defaultOptions
  , defaultRulesLocations
  , intToVerbosity
  )
  where

import Data.List.NonEmpty (NonEmpty, fromList, singleton)
import System.FilePath ((</>), (<.>))


data PubYear
   = AnyDate      -- Default
      -- Use any date found in this order: Issued, Created, Epub (date element), Modified
   | NoModified   -- Don't use Modified date: Issued, Created, Epub (date element)
   | NoDate       -- Don't do publication date at all
  deriving Show  -- FIXME

newtype InteractiveSwitch = InteractiveSwitch { v :: Bool }
  deriving Show  -- FIXME

newtype NoActionSwitch = NoActionSwitch { v :: Bool }
  deriving Show  -- FIXME

newtype PublisherSwitch = PublisherSwitch { v :: Bool }
  deriving Show  -- FIXME

newtype RulesLocations = RulesLocations (NonEmpty RulesLocation)
  deriving Show  -- FIXME

data RulesLocation
  = RulesPath FilePath
  | RulesViaEnv String FilePath
  | BuiltinRules
  deriving Show  -- FIXME

newtype MoveSwitch = MoveSwitch { v :: Bool }
  deriving Show  -- FIXME

data VerbosityLevel = Normal | ShowFormatter | ShowBookInfo
  deriving (Eq, Ord, Show)  -- FIXME

newtype DumpRulesSwitch = DumpRulesSwitch { v :: Bool }
  deriving Show  -- FIXME

newtype HelpRulesSwitch = HelpRulesSwitch { v :: Bool }
  deriving Show  -- FIXME

newtype BookFiles = BookFiles { v :: NonEmpty FilePath }
  deriving Show  -- FIXME

data Options = Options
  { pubYear           :: PubYear
  , interactive       :: InteractiveSwitch
  , noAction          :: NoActionSwitch
  , includePublisher  :: PublisherSwitch
  , rulesPaths        :: RulesLocations
  , optTargetDir      :: FilePath  -- FIXME Wrap in a newtype like BookFiles
  , move              :: MoveSwitch
  , verbosityLevel    :: VerbosityLevel
  , dumpRules         :: DumpRulesSwitch
  , helpRules         :: HelpRulesSwitch
  , bookFiles         :: BookFiles
  }
  deriving Show  -- FIXME


intToVerbosity :: Int -> VerbosityLevel
intToVerbosity 0 = Normal
intToVerbosity 1 = ShowFormatter
intToVerbosity _ = ShowBookInfo


defaultRulesFile :: FilePath
defaultRulesFile = "default" <.> "rules"


defaultRulesLocations :: RulesLocations
defaultRulesLocations = RulesLocations $ fromList
  [ RulesViaEnv "HOME" $ ".config" </> "epubtools" </> defaultRulesFile   -- UNIX-like
  , RulesViaEnv "HOME" $ ".epubtools" </> defaultRulesFile                -- UNIX-like, old-school
  , RulesViaEnv "APPDATA" $ "epubtools" </> defaultRulesFile              -- Windows
  , BuiltinRules
  ]


defaultOptions :: Options
defaultOptions = Options
  { pubYear           = AnyDate
  , interactive       = InteractiveSwitch False
  , noAction          = NoActionSwitch False
  , includePublisher  = PublisherSwitch False
  , rulesPaths        = defaultRulesLocations
  , optTargetDir      = "."
  , move              = MoveSwitch False
  , verbosityLevel    = Normal
  , dumpRules         = DumpRulesSwitch False
  , helpRules         = HelpRulesSwitch False
  , bookFiles         = BookFiles (singleton "dummy-filename")
  }
