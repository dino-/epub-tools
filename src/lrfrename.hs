#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Data.Either
import Data.Map
import Data.Maybe
import HSH.Command
import Prelude hiding ( lookup )
import System.Environment
--import System.Posix.Process
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


parseLine :: (MonadState [Map String String] m) => String -> m ()

parseLine "-----" = do
   rs <- get
   put (empty:rs)

parseLine line = do
   let p = parseTag line
   unless (p == Nothing) $ do
      (r:rs) <- get
      let r' = (\(Just (k, v)) -> insert k v r) p
      put (r':rs)


main :: IO ()
main = do
   paths <- getArgs

   result <- tryEC $ run $ "lrf-meta " ++ (head paths)
   either (print) (putStrLn) result

   putStrLn "\ndone"

{-
   allLines <- liftM lines getContents

   let result = execState (mapM parseLine allLines) []
   --print result
   --print $ head result
   print $ take 5 result

   return ()
-}
