-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module TestSimple (
   testSimple
)
   where

import Test.HUnit ( Test (..), assertBool, assertString )
--import Text.Regex.Posix ( (=~) )


{- Test foo bar baz
-}


testSimple :: Test
testSimple = TestList
   [ TestLabel "testOne" testOne
   ]


testOne :: Test
testOne = TestCase $ assertBool "testOne description" False


{-
topDir = Util.resourcesPath </> "foo"
oldPath = Util.resourcesPath </> "img_1220.jpg"
newLinkPath = topDir </> "2003/2003-09-02/20030902_220.jpg"


testLink :: Test
testLink = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput [ topDir, oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPath
   assertBool "make link: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool "make link: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive topDir

   -- Test output to stdout
   assertBool "make link: correct output"
      (output =~ newLinkPath :: Bool)
-}
