{-# LANGUAGE DuplicateRecordFields #-}

module EpubTools.EpubName.Common
  ( BookFiles (..), InteractiveSwitch (..), MoveSwitch (..)
  , NoActionSwitch (..), Options (..), PublisherSwitch (..), PubYear (..)
  , RulesLocation (..), RulesLocations (..)
  , TargetDirs (..), VerbosityLevel (..)
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

newtype InteractiveSwitch = InteractiveSwitch { v :: Bool }

newtype NoActionSwitch = NoActionSwitch { v :: Bool }

newtype PublisherSwitch = PublisherSwitch { v :: Bool }

newtype RulesLocations = RulesLocations (NonEmpty RulesLocation)

data RulesLocation
  = RulesPath FilePath
  | RulesViaEnv String FilePath
  | BuiltinRules

newtype TargetDirs = TargetDirs { v :: NonEmpty FilePath }

newtype MoveSwitch = MoveSwitch { v :: Bool }

data VerbosityLevel = Normal | ShowFormatter | ShowBookInfo
  deriving (Eq, Ord)

newtype BookFiles = BookFiles { v :: NonEmpty FilePath }

data Options = Options
  { pubYear           :: PubYear
  , interactive       :: InteractiveSwitch
  , noAction          :: NoActionSwitch
  , includePublisher  :: PublisherSwitch
  , rulesPaths        :: RulesLocations
  , targetDirs        :: TargetDirs
  , move              :: MoveSwitch
  , verbosityLevel    :: VerbosityLevel
  , bookFiles         :: BookFiles
  }


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
  , targetDirs        = TargetDirs (singleton ".")
  , move              = MoveSwitch False
  , verbosityLevel    = Normal
  , bookFiles         = BookFiles (singleton "dummy-filename")
  }
