module BookName.Formatters
   ( tryFormatting )
   where

import Control.Monad.Error
import Data.Map hiding ( filter, map, null )
import Data.Maybe ( fromJust )
import Prelude hiding ( lookup )
import Text.Printf

import BookName.Format.Anonymous
import BookName.Format.AuthorBasic
import BookName.Format.AuthorDouble
import BookName.Format.AuthorSt
import BookName.Format.AuthorThird
import BookName.Format.MagAeon
import BookName.Format.MagChallengingDestiny
import BookName.Format.MagDell
import BookName.Format.MagFsf
import BookName.Format.MagFutureOrbits
import BookName.Format.MagGud
import BookName.Format.MagInterzone
import BookName.Format.MagNameIssue
import BookName.Format.MagNemesis
import BookName.Format.MagSomethingWicked
import BookName.Format.SFBestOf
import BookName.Util ( Fields )


formatters :: [Fields -> ErrorT String IO (String, String)]
formatters =
   [ fmtMagDell
   , fmtMagNemesis
   , fmtMagAeon
   , fmtMagChallengingDestiny
   , fmtMagGud
   , fmtMagInterzone
   , fmtMagSomethingWicked
   , fmtMagFsf
   , fmtMagFutureOrbits
   , fmtMagNameIssue
   , fmtSFBestOf
   , fmtAuthorDouble
   , fmtAnonymous
   , fmtAuthorThird
   , fmtAuthorSt
   , fmtAuthorBasic
   ]


tryFormatting :: Fields -> ErrorT String IO (String, String)
tryFormatting fields = do
   let oldPath = fromJust $ lookup "File" fields
   foldr mplus 
      (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
      map (\f -> f fields) formatters
