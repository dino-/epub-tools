-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Util
   ( EM , runEM
   , liftIO
   , throwError
   )
   where

import Control.Monad.Error


type EM a = (ErrorT String IO) a

runEM :: EM a -> IO (Either String a)
runEM = runErrorT