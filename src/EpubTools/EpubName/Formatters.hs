-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Formatters
   ( tryFormatting )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad
import Text.Printf

--import EpubTools.EpubName.Format.Anonymous
import EpubTools.EpubName.Format.AuthorBasic
import EpubTools.EpubName.Format.AuthorDouble
--import EpubTools.EpubName.Format.AuthorSt
--import EpubTools.EpubName.Format.AuthorThird
import EpubTools.EpubName.Format.MagAeon
import EpubTools.EpubName.Format.MagAnalog
import EpubTools.EpubName.Format.MagApex
import EpubTools.EpubName.Format.MagBcs
import EpubTools.EpubName.Format.MagChallengingDestiny
import EpubTools.EpubName.Format.MagClarkesworld
import EpubTools.EpubName.Format.MagEclipse
import EpubTools.EpubName.Format.MagFsf
import EpubTools.EpubName.Format.MagFutureOrbits
import EpubTools.EpubName.Format.MagGud
import EpubTools.EpubName.Format.MagInterzone
import EpubTools.EpubName.Format.MagLightspeedDate
import EpubTools.EpubName.Format.MagLightspeedIssue
import EpubTools.EpubName.Format.MagNameIssue
import EpubTools.EpubName.Format.MagNemesis
import EpubTools.EpubName.Format.MagRageMachine
import EpubTools.EpubName.Format.MagSomethingWicked
import EpubTools.EpubName.Format.MagUniverse
import EpubTools.EpubName.Format.MagWeirdTales
import EpubTools.EpubName.Format.SFBestOf
import EpubTools.EpubName.Util


formatters :: [Metadata -> EN (String, String)]
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
   , fmtMagClarkesworld
   , fmtMagGud
   , fmtMagInterzone
   , fmtMagLightspeedDate
   , fmtMagLightspeedIssue
   , fmtMagSomethingWicked
   , fmtMagRageMachine
   , fmtMagFsf
   , fmtMagFutureOrbits
   , fmtMagBcs
   , fmtMagApex
   , fmtMagNameIssue
   , fmtMagUniverse
   , fmtMagWeirdTales
   , fmtSFBestOf
   , fmtAuthorDouble

   , fmtAuthorBasic
   ]


tryFormatting :: (FilePath, Metadata) -> EN (String, String)
tryFormatting (oldPath, md) = do
   foldr mplus 
      (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
      map (\f -> f md) formatters
