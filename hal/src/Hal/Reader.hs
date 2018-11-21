
module Hal.Reader
(
  halRead
) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Void
import Text.Megaparsec (Parsec, parse, (<|>))

import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Hal.Types

type Parser = Parsec Void String

halRead :: String -> Either String HalValue
halRead s = bimap P.parseErrorPretty id $ parse parseHalValue "" (dropWhile isHalWhiteSpace s)

isHalWhiteSpace :: Char -> Bool
isHalWhiteSpace c = isSpace c || c == ','

isHalSymbolChar :: Char -> Bool
isHalSymbolChar c = c `notElem` " ,;(){}[]\"~@`"

parseHalValue :: Parser HalValue
parseHalValue =  lexeme go
  where go = parseQuote
         <|> parseQuasiQuote
         <|> P.try parseSpliceUnquote
         <|> parseUnquote
         <|> parseKeyword
         <|> P.try parseInt
         <|> parseString
         <|> parseList
         <|> parseVector
         <|> parseHashMap
         <|> parseMeta
         <|> parseDeref
         <|> parseSymbol

whiteSpace :: Parser ()
whiteSpace = void $ P.takeWhile1P (Just "white space") isHalWhiteSpace

comments :: Parser ()
comments = L.skipLineComment ";"

space :: Parser ()
space = L.space whiteSpace comments P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

parseInt :: Parser HalValue
parseInt = HalInt <$> p
  where
    p = L.signed (pure ()) L.decimal <* P.notFollowedBy follow
    follow = P.satisfy (\c -> c /= '\'' && isHalSymbolChar c)

parseSymbol :: Parser HalValue
parseSymbol = do
  s <- lexeme (P.takeWhile1P (Just "symbol") isHalSymbolChar)
  case s of
    "nil" -> pure HalNil
    "true" -> pure HalTrue
    "false" -> pure HalFalse
    _ -> pure $ HalSymbol s

parseList :: Parser HalValue
parseList = HalList <$> P.between (symbol "(") (symbol ")") (P.many parseHalValue)

parseString :: Parser HalValue
parseString = HalString <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

parseMacro :: String -> String -> Parser HalValue
parseMacro sym form = do
  void $ symbol sym
  val <- parseHalValue
  pure $ HalList [HalSymbol form, val]

parseQuote :: Parser HalValue
parseQuote = parseMacro "'" "quote"

parseQuasiQuote :: Parser HalValue
parseQuasiQuote = parseMacro "`" "quasiquote"

parseUnquote :: Parser HalValue
parseUnquote = parseMacro "~" "unquote"

parseSpliceUnquote :: Parser HalValue
parseSpliceUnquote = parseMacro "~@" "splice-unquote"

parseDeref :: Parser HalValue
parseDeref = parseMacro "@" "deref"

parseKeyword :: Parser HalValue
parseKeyword = do
  void $ P.char ':'
  HalKeyword <$> lexeme (P.takeWhile1P (Just "symbol") isHalSymbolChar)

parseVector :: Parser HalValue
parseVector = HalVector . Seq.fromList <$> P.between (symbol "[") (symbol "]") (P.many parseHalValue)

parseHashMap :: Parser HalValue
parseHashMap = do
  void $ symbol "{"
  kvs <- P.many ((,) <$> lexeme parseHalValue <*> lexeme parseHalValue)
  void $ symbol "}"
  pure . HalHashMap . HM.fromList $ kvs

parseMeta :: Parser HalValue
parseMeta = do
  void $ symbol "^"
  meta <- parseHashMap
  val <- parseHalValue
  pure $ HalList [HalSymbol "with-meta", val, meta]

