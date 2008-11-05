#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Data.Maybe
import Text.Regex


data LrfMeta = LrfMeta
   { lrfmFile :: Maybe String
   , lrfmAuthor :: Maybe String
   , lrfmAuthorReading :: Maybe String
   , lrfmBookId :: Maybe String
   , lrfmCategory :: Maybe String
   , lrfmClassification :: Maybe String
   , lrfmCreationDate :: Maybe String
   , lrfmCreator :: Maybe String
   , lrfmFreeText :: Maybe String
   , lrfmLabel :: Maybe String
   , lrfmLanguage :: Maybe String
   , lrfmSumPage :: Maybe String
   , lrfmProducer :: Maybe String
   , lrfmPublisher :: Maybe String
   , lrfmTitle :: Maybe String
   , lrfmTitleReading :: Maybe String
   }
   deriving Show

defaultMeta :: LrfMeta
defaultMeta = LrfMeta
   { lrfmFile = Nothing
   , lrfmAuthor = Nothing
   , lrfmAuthorReading = Nothing
   , lrfmBookId = Nothing
   , lrfmCategory = Nothing
   , lrfmClassification = Nothing
   , lrfmCreationDate = Nothing
   , lrfmCreator = Nothing
   , lrfmFreeText = Nothing
   , lrfmLabel = Nothing
   , lrfmLanguage = Nothing
   , lrfmSumPage = Nothing
   , lrfmProducer = Nothing
   , lrfmPublisher = Nothing
   , lrfmTitle = Nothing
   , lrfmTitleReading = Nothing
   }


parseTag :: LrfMeta -> String -> LrfMeta
parseTag r line = case (matchResults line) of
   ("File", v) -> r { lrfmFile = v }
   ("Author", v) -> r { lrfmAuthor = v }
   ("Author.reading", v) -> r { lrfmAuthorReading = v }
   ("BookID", v) -> r { lrfmBookId = v }
   ("Category", v) -> r { lrfmCategory = v }
   ("Classification", v) -> r { lrfmClassification = v }
   ("CreationDate", v) -> r { lrfmCreationDate = v }
   ("Creator", v) -> r { lrfmCreator = v }
   ("FreeText", v) -> r { lrfmFreeText = v }
   ("Label", v) -> r { lrfmLabel = v }
   ("Language", v) -> r { lrfmLanguage = v }
   ("SumPage", v) -> r { lrfmSumPage = v }
   ("Producer", v) -> r { lrfmProducer = v }
   ("Publisher", v) -> r { lrfmPublisher = v }
   ("Title", v) -> r { lrfmTitle = v }
   ("Title.reading", v) -> r { lrfmTitleReading = v }
   _           -> r

   where
      matchResults l =
         case (fromJust $ matchRegex (mkRegex "(.*): (.*)") l) of
            [key, ""] -> (key, Nothing)
            [key, val] -> (key, Just val)


parseLine :: (MonadState [LrfMeta] m) => String -> m ()

parseLine "-----" = do
   rs <- get
   put (defaultMeta:rs)

parseLine line = do
   (r:rs) <- get
   let r' = parseTag r line
   put (r':rs)


sampleFileContents = "-----\nFile: foo\nShit: box\n-----\nFile: bar\n-----\nFile: baz\n"


main :: IO ()
main = do
   --let allLines = lines sampleFileContents
   allLines <- liftM lines getContents

   let result = execState (mapM parseLine allLines) []
   --print result
   print $ head result

   return ()
