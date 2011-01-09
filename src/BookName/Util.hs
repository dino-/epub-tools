-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module BookName.Util
   ( runBN
   )
   where

import Control.Monad.Error


--type BN a = (ErrorT String IO) a

runBN :: (ErrorT e m) a -> m (Either e a)
runBN = runErrorT
