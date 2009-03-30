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
import BookName.Util


assertNewName :: (Eq a, Show a) =>
                 String
                 -> (Fields -> Control.Monad.Error.ErrorT a IO a)
                 -> [String]
                 -> a
                 -> Assertion
assertNewName desc fmtFunction meta expected = do
   let fields = parseMeta "foo" $ unlines meta
   result <- runBN $ fmtFunction fields
   let actual = either id id result
   assertEqual desc expected actual
