module BookName.Formatters
   ( tryFormatting )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

--import BookName.Format.Anonymous
import BookName.Format.AuthorBasic
import BookName.Format.AuthorDouble
--import BookName.Format.AuthorSt
--import BookName.Format.AuthorThird
import BookName.Format.MagAeon
import BookName.Format.MagAnalog
import BookName.Format.MagApex
import BookName.Format.MagBcs
import BookName.Format.MagChallengingDestiny
import BookName.Format.MagEclipse
import BookName.Format.MagFsf
import BookName.Format.MagFutureOrbits
import BookName.Format.MagGud
import BookName.Format.MagInterzone
import BookName.Format.MagLightspeed
import BookName.Format.MagNameIssue
import BookName.Format.MagNemesis
import BookName.Format.MagRageMachine
import BookName.Format.MagSomethingWicked
import BookName.Format.SFBestOf


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
