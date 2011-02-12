#! /usr/bin/env runhaskell

-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Distribution.Simple
import System.Cmd ( system )


main = defaultMainWithHooks (simpleUserHooks 
   { runTests = testRunner
   } )

   where
      -- Target for running all unit tests
      testRunner _ _ _ _ = do
         system $ "runhaskell -isrc testsuite/runtests.hs"
         return ()
