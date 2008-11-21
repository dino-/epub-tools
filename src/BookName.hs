{-# LANGUAGE FlexibleContexts #-}

module BookName
   where

import Control.Monad.Error
import Data.Char
import Data.Map hiding ( filter, map )
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
   [ (\s -> subRegex (mkRegex "[.',\\?();#]") s "")
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
