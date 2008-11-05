#! /usr/bin/env runhaskell

import Data.Either
import Data.Map hiding ( map )
import Data.Maybe
import HSH.Command
import Prelude hiding ( lookup )
import System.Environment
import Text.Regex


allKeys :: [String]
allKeys =
   [ "File"
   , "Author"
   , "Author.reading"
   , "BookID"
   , "Category"
   , "Classification"
   , "CreationDate"
   , "Creator"
   , "FreeText"
   , "Label"
   , "Language"
   , "SumPage"
   , "Producer"
   , "Publisher"
   , "Title"
   , "Title.reading"
   ]


parseTag :: String -> Maybe (String, String)
parseTag line =
   case (fromJust $ matchRegex (mkRegex "(.*): (.*)") line) of
      [_  , "" ] -> Nothing
      [key, val] -> Just (key, val)


parseBook :: String -> Map String String
parseBook raw = fromList $ catMaybes $ map parseTag $ lines raw


main :: IO ()
main = do
   paths <- getArgs

   result <- tryEC $ run $ "lrf-meta " ++ (head paths)
   --either (print) (putStrLn) result
   either (print) (print . parseBook) result

   putStrLn "\ndone"
