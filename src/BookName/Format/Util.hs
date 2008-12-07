{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.Util
   where

import Control.Monad.Error
import Data.Char
import Data.Map hiding ( filter, map )
import Data.Maybe
import Prelude hiding ( lookup )
import Text.Regex


type Fields = Map String String


lookupE :: (MonadError String m) => String -> Map String a -> m a
lookupE k m = case (lookup k m) of
   Nothing -> throwError $ "[ERROR missing key: " ++ k ++ "]"
   Just v -> return v


capFirstAndDeSpace :: String -> String
capFirstAndDeSpace s = concat $ map capFirst $ words s
   where
      capFirst (first:rest) = (toUpper first) : rest
      capFirst _ = undefined


commonFilters :: [(String -> String)]
commonFilters =
   [ (\s -> subRegex (mkRegex "[.',\\?();#:]") s "")
   , filter (/= '"')
   , (\s -> subRegex (mkRegex "]") s "")
   , (\s -> subRegex (mkRegex "\\*") s "")
   , (\s -> subRegex (mkRegex "!") s "")
   , (\s -> subRegex (mkRegex "-") s " ")
   , (\s -> subRegex (mkRegex "\\[") s "_")
   , (\s -> subRegex (mkRegex "^The ") s "")
   , (\s -> subRegex (mkRegex "&") s " And ")
   , capFirstAndDeSpace
   ]


extractYear :: Maybe String -> String
extractYear Nothing   = ""
extractYear (Just ft) =
   case (matchRegex (mkRegex ".*([0-9]{4}).*") ft) of
      Just (y:_) -> '_' : y
      _          -> ""


formatAuthor :: (MonadError String m) =>
   [(String, [String] -> a)] -> String -> m a
formatAuthor authorPatterns author = do
   let (matchResult, formatter) =
         foldr f (Nothing, Nothing) mkMatchExprs
   case formatter of
      Nothing -> throwError "No handler found"
      Just g -> return $ g $ fromJust matchResult

   where
      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, Just i))
            authorPatterns


formatTitle :: (MonadError String m) => 
   [(String, String -> [String] -> a)] -> String -> String -> m a
formatTitle titlePatterns year author = do
   let (matchResult, formatter) =
         foldr f (Nothing, Nothing) mkMatchExprs
   case formatter of
      Nothing -> throwError "No handler found"
      Just g -> return $ g year $ fromJust matchResult

   where
      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, Just i))
            titlePatterns
