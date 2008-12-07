{-# LANGUAGE FlexibleContexts #-}

module BookName.Util
   where

import Control.Monad.Error
--import Data.Map hiding ( filter, map )
import Data.Map
--import Prelude hiding ( lookup )


--type BN a = (ErrorT String IO) a


runBN :: (ErrorT e m) a -> m (Either e a)
runBN = runErrorT


type Fields = Map String String
