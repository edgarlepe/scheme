module Lib
    ( symbol
    , readExp
    , spaces
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readHex, readOct)
import Data.Char (digitToInt)
import Control.Monad ((>=>))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool

readExp :: String -> String
readExp input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _  -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseChar

parseAtom :: Parser LispVal
parseAtom =
  do first <- letter <|> symbol
     rest <- many $ letter <|> digit <|> symbol
     let atom = first:rest
     return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseString :: Parser LispVal
parseString =
  do _ <- char '"'
     x <- many $ escapedChars <|> noneOf "\""
     _ <- char '"'
     return $ String x

parseNumber :: Parser LispVal
parseNumber = parseDec
          <|> parseDec2
          <|> parseOct
          <|> parseHex
          <|> parseBin

parseOct :: Parser LispVal
parseOct = try (string "#o") >> many1 octDigit >>= return . Number . octToDig
  where octToDig = fst . head . readOct

parseDec :: Parser LispVal
parseDec = Number . read <$> many1 digit

parseDec2 :: Parser LispVal
parseDec2 = try (string "#d") >> Number . read <$> many1 digit

parseHex :: Parser LispVal
parseHex = try (string "#x") >> many1 hexDigit >>= return . Number . hexToDig
  where hexToDig = fst . head . readHex

parseBin :: Parser LispVal
parseBin =
  try (string "#b") >>
  many1 (oneOf "10") >>=
  return . Number . binToDig
-- x0 + 2 * (x1 + 2 * (x2 + 2 * (x3 + 2 * x4)))
  where binToDig "" = 0
        binToDig s = foldr f 0 ds
          where ds      = (toInteger . digitToInt) <$> reverse s
                f x acc = x + 2 * acc

parseChar :: Parser LispVal
parseChar =
  do _ <- try (string "#\\")
     x <- anyChar >>= \c -> notFollowedBy alphaNum >> return c
     return . Char $ x

parseChar2 :: Parser LispVal
parseChar2 =
  try (string "#\\") >>
  try (string "newline" <|> string "space") >>= \x ->
  return . Char $ case x of
                    "newline" -> '\n'
                    "space"   -> ' '

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = char '\\' >>
               oneOf "\\\"\n\r\t" >>= \x ->
               return $ case x of
                          '\\' -> x
                          '"'  -> x
                          'n'  -> '\n'
                          'r'  -> '\r'
                          't'  -> '\t'
