#! /usr/bin/env runhaskell

import Control.Monad ( unless )
import Distribution.Simple
import System.Cmd
import System.FilePath
import System.Posix.Files ( createSymbolicLink, fileExist )


main = defaultMainWithHooks (simpleUserHooks 
   { postBuild = customPostBuild
   , runTests = testRunner
   } )

   where
      -- Create symlink to the binary after build for developer 
      -- convenience
      customPostBuild _ _ _ _ = do
         let dest = "bookname"

         exists <- fileExist dest
         unless exists $ do
            let src = "dist" </> "build" </> dest </> dest
            createSymbolicLink src dest

      -- Target for running all unit tests
      testRunner _ _ _ _ = do
         system $ "runhaskell -itestsuite testsuite/runtests.hs"
         return ()
