-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Util
   ( EN , runEN
   , throwError
   , asks
   )
   where

import Control.Monad.Error
import Control.Monad.Reader

import EpubTools.EpubName.Opts


type EN a = ReaderT Options (ErrorT String IO) a

runEN :: Options -> EN a -> IO (Either String a)
runEN env ev = runErrorT $ runReaderT ev env
