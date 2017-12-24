#! /usr/bin/env runhaskell

import Data.List ( isPrefixOf )
import System.Cmd
import System.Directory
import System.FilePath
import Text.Printf
import Text.Regex


--epubnameBin = return "epubname"
epubnameBin = canonicalizePath "dist/build/epubname/epubname"

tempDir = "/home/dino/temp"
allFile = "epubname-all"


renameRE = mkRegex "(.*) -> (.*)"

namesNotEqual oline = case matchRegex renameRE oline of
   Just [s, d] -> takeFileName s /= takeFileName d
   Nothing     -> True


main :: IO ()
main = do
   eb <- epubnameBin

   setCurrentDirectory "/var/local/archive/doc/books/fiction"

   system $ printf
      "find . -name '*.epub' | xargs -n 30 %s -n -t /tmp 2>&1 > %s/%s"
      eb tempDir allFile

   setCurrentDirectory tempDir

   allLines <- fmap (filter (not . isPrefixOf "No-action") . lines)
      $ readFile allFile

   let notEqual = filter namesNotEqual allLines
   writeFile "epubname-changed" $ unlines notEqual

   mapM_ putStrLn notEqual
