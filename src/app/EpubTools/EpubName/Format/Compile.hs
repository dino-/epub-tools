-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module EpubTools.EpubName.Format.Compile
   where

import Data.Maybe ( catMaybes )
import Text.ParserCombinators.Parsec

import EpubTools.EpubName.Format.Author
import EpubTools.EpubName.Format.Format


parseRules :: SourceName -> String -> Either ParseError [Formatter]
parseRules name = runParser pFormatters () name


pFormatters :: Parser [Formatter]
pFormatters = fmap catMaybes $ many pFormatter


pFormatter :: Parser (Maybe Formatter)
pFormatter = do
   l <- many1Till (noneOf " ") eol
   a <- optionMaybe $ try $ pCommand "authorMatch"
   s <- optionMaybe $ try $ pCommand "subjectMatch"
   t <- pCommand "titlePat"
   n <- pName
   newline <|> return ' '
   return $ case head l of
      '!' -> Nothing
      _   -> Just $ Formatter l
         [ maybe (return ()) authorMatches a
         , maybe (return ()) subjectMatches s
         ]
         (extractTitle t) n

   
pName :: Parser [ReplF]
pName = do
   pCommandPrefix "name"
   fs <- pRepls
   eol
   return fs


pCommand :: String -> Parser String
pCommand c = do
   pCommandPrefix c
   char '"'
   qs <- manyTill (noneOf "\"") (char '"')
   eol
   return qs


pCommandPrefix :: String -> Parser ()
pCommandPrefix c = do
   many1 space
   string c
   spaces
   return ()


{-
pRepls :: Parser [[String]]
pRepls = do
   char '"'
   rs <- many pExpr
   char '"'
   return rs
-}
pRepls :: Parser [ReplF]
pRepls = do
   char '"'
   rs <- many pExpr
   char '"'

   let extension = literal ".epub"
   return $ rs ++ [publisher, extension]


{-
pExpr :: Parser [String]
pExpr = pRepl <|> pLiteral
-}
pExpr :: Parser ReplF
pExpr = pRepl <|> pLiteral


--pRepl :: Parser [String]
pRepl :: Parser ReplF
pRepl = do
   char '('
   spaces
   x <- pCall <|> pIndex
   spaces
   char ')'
   return x


{-
pCall :: Parser [String]
pCall = do
   fn <- pFName
   spaces
   args <- pArgList
   return $ fn : args
-}
pCall :: Parser ReplF
pCall = do
   fn <- pFName
   spaces
   args <- pArgList
   mkReplF $ fn : args


pFName :: Parser String
pFName = do
   first <- letter
   rest <- many alphaNum
   return $ first : rest


pArgList :: Parser [String]
pArgList = sepEndBy (many1 digit) (char ' ')


{-
pIndex :: Parser [String]
pIndex = do
   ds <- many1 digit
   return [ds]
-}
pIndex :: Parser ReplF
pIndex = do
   ds <- many1 digit
   mkReplF ["index", ds]


{-
pLiteral :: Parser [String]
pLiteral = do
   l <- many1 (noneOf "()\"")
   return [l]
-}
pLiteral :: Parser ReplF
pLiteral = do
   l <- many1 (noneOf "()\"")
   mkReplF ["literal", l]


many1Till :: Parser a -> Parser end -> Parser [a]
many1Till p end = do
   h <- p
   t <- manyTill p end
   return $ h : t


eol :: Parser Char
eol = newline <|> (eof >> return '\n')


mkReplF :: [String] -> Parser ReplF
mkReplF ("authors" : []) = return authors
mkReplF ("index" : i : []) = return $ index i
mkReplF ("literal" : s : []) = return $ literal s
mkReplF ("monthNum" : i : []) = return $ monthNum i
mkReplF ("pad" : w : i : []) = return $ pad w i
mkReplF ("scrub" : i : []) = return $ scrub i
mkReplF ("wordNum" : w : i : []) = return $ wordNum w i
mkReplF ("year" : []) = return year
mkReplF l = unexpected $ "Bad function definition: " ++ (concat l)
