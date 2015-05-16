-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Util
   ( EM , runEM
   , liftIO
   , throwError
   )
   where

import Control.Monad.Except


type EM a = (ExceptT String IO) a

runEM :: EM a -> IO (Either String a)
runEM = runExceptT
