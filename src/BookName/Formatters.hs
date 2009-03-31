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
import BookName.Format.MagAeon
import BookName.Format.MagApex
import BookName.Format.MagChallengingDestiny
import BookName.Format.MagDell
import BookName.Format.MagGud
import BookName.Format.MagNemesis
import BookName.Util ( Fields )


formatters :: [Fields -> ErrorT String IO String]
formatters =
   [ fmtMagDell
   , fmtMagNemesis
   , fmtMagAeon
   , fmtMagApex
   , fmtMagChallengingDestiny
   , fmtMagGud
   , fmtAuthorDouble
   , fmtAnonymous
   , fmtAuthorBasic
   ]


tryFormatting :: Fields -> ErrorT String IO String
tryFormatting fields = do
   let oldPath = fromJust $ lookup "File" fields
   foldr mplus 
      (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
      map (\f -> f fields) formatters
