-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubName.Formatters
   ( tryFormatting )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

--import EpubName.Format.Anonymous
import EpubName.Format.AuthorBasic
import EpubName.Format.AuthorDouble
--import EpubName.Format.AuthorSt
--import EpubName.Format.AuthorThird
import EpubName.Format.MagAeon
import EpubName.Format.MagAnalog
import EpubName.Format.MagApex
import EpubName.Format.MagBcs
import EpubName.Format.MagChallengingDestiny
import EpubName.Format.MagEclipse
import EpubName.Format.MagFsf
import EpubName.Format.MagFutureOrbits
import EpubName.Format.MagGud
import EpubName.Format.MagInterzone
import EpubName.Format.MagLightspeed
import EpubName.Format.MagNameIssue
import EpubName.Format.MagNemesis
import EpubName.Format.MagRageMachine
import EpubName.Format.MagSomethingWicked
import EpubName.Format.SFBestOf


formatters :: [Metadata -> ErrorT String IO (String, String)]
formatters =
{-
   , fmtAnonymous
   , fmtAuthorThird
   , fmtAuthorSt
-}
   [ fmtMagAnalog
   , fmtMagNemesis
   , fmtMagAeon
   , fmtMagEclipse
   , fmtMagChallengingDestiny
   , fmtMagGud
   , fmtMagInterzone
   , fmtMagLightspeed
   , fmtMagSomethingWicked
   , fmtMagRageMachine
   , fmtMagFsf
   , fmtMagFutureOrbits
   , fmtMagBcs
   , fmtMagApex
   , fmtMagNameIssue
   , fmtSFBestOf
   , fmtAuthorDouble

   , fmtAuthorBasic
   ]


tryFormatting :: (FilePath, Metadata) -> ErrorT String IO (String, String)
tryFormatting (oldPath, md) = do
   foldr mplus 
      (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
      map (\f -> f md) formatters
