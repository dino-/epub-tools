-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.Util
   ( assertNewName )
   where

import Control.Monad.Error
import Test.HUnit ( Test (..), assertEqual )
import Test.HUnit.Base ( Assertion )

import BookName.Extract
import BookName.Formatters ( tryFormatting )
import BookName.Util


assertNewName :: String
                 -> [String]
                 -> String
                 -> Assertion
assertNewName desc meta expected = do
   let fields = parseMeta ("unit test: " ++ desc) $ unlines meta
   result <- runBN $ tryFormatting fields
   let actual = either id id result
   assertEqual desc expected actual
