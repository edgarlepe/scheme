-- Parser.hs

module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readHex, readOct, readFloat)
import Data.Char (digitToInt)
import qualified Data.Complex as C
import Data.Ratio ((%), numerator, denominator)
import Data.Vector (Vector, fromList, toList)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Vector LispVal)
             | Complex (C.Complex Double)
             | Integer Integer
             | Real Double
             | Rational Rational
             | String String
             | Char Char
             | Bool Bool


instance Show LispVal where
  show (String s)       = "\"" ++ s ++ "\""
  show (Atom x)         = x
  show (Bool True)      = "#t"
  show (Bool False)     = "#f"
  show (List xs)        = "(" ++ unwordsList xs ++ ")"
  show (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
  show (Integer n)      = show n
  show (Complex c)      = show (C.realPart c) ++ case C.imagPart c of
                                               0 -> ""
                                               i -> "+" ++ show i ++ "i"
  show (Real r)           = show r
  show (Rational r)       = show (numerator r) ++ "/" ++ show (denominator r)
  show (Char c)           = "#\\" ++ show c
  show (Vector v)         = "#(" ++ (unwordsList . toList) v ++ ")"

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err   -> "No match: " ++ show err
    Right val  -> "Found " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseUnquote
        <|> try parseVector
        <|> parseAtom
        <|> try parseString
        <|> parseNumber
        <|> try parseChar
        <|> try parseQuoted
        <|> do _ <- char '('
               x <- try parseList <|> try parseDottedList
               _ <- char ')'
               return x
        <|> parseQuasiQuoted

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
parseNumber = try parseComplex
          <|> try parseReal
          <|> try parseRational
          <|> try parseInteger

parseInteger :: Parser LispVal
parseInteger = parseDec
           <|> parseDec2
           <|> parseOct
           <|> parseHex
           <|> parseBin

parseOct :: Parser LispVal
parseOct = try (string "#o") >> many1 octDigit >>= return . Integer . octToDig
  where octToDig = fst . head . readOct

parseDec :: Parser LispVal
parseDec = Integer . read <$> many1 digit

parseDec2 :: Parser LispVal
parseDec2 = try (string "#d") >> Integer . read <$> many1 digit

parseHex :: Parser LispVal
parseHex = try (string "#x") >> many1 hexDigit >>= return . Integer . hexToDig
  where hexToDig = fst . head . readHex

parseBin :: Parser LispVal
parseBin =
  try (string "#b") >>
  many1 (oneOf "10") >>=
  return . Integer . binToDig
-- x0 + 2 * (x1 + 2 * (x2 + 2 * (x3 + 2 * x4)))
  where binToDig "" = 0
        binToDig s = foldr f 0 ds
          where ds      = (toInteger . digitToInt) <$> reverse s
                f x acc = x + 2 * acc

parseChar :: Parser LispVal
parseChar = parseChar1 <|> parseChar2

parseChar1 :: Parser LispVal
parseChar1 =
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

parseReal :: Parser LispVal
parseReal =
  do x <- many1 digit
     _ <- char '.'
     y <- many1 digit
     return . Real . fst . head . readFloat $ x ++ ['.'] ++ y

parseRational :: Parser LispVal
parseRational =
  do n <- many1 digit
     _ <- char '/'
     d <- many1 digit
     return . Rational $ read d % read n

parseComplex :: Parser LispVal
parseComplex = try parseComplex1 <|> try parseComplex2

parseComplex1 :: Parser LispVal
parseComplex1 =
  do r <- parseDec
     _ <- char '+'
     i <- parseDec
     _ <- char 'i'
     return . Complex $ (toDouble r) C.:+ (toDouble i)

parseComplex2 :: Parser LispVal
parseComplex2 =
  do r <- parseReal
     _ <- char '+'
     i <- parseReal
     _ <- char 'i'
     return . Complex $ (toDouble r) C.:+ (toDouble i)

toDouble :: Fractional a => LispVal -> a
toDouble (Real f)    = realToFrac f
toDouble (Integer f) = fromIntegral f

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList =
  do h <- endBy parseExpr spaces
     t <- char '.' >> spaces >> parseExpr
     return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted =
  do _ <- char '\''
     x <- parseExpr
     return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted =
  do _ <- char '`'
     e <- parseExpr
     return $ List [Atom "quasiquote", e]

parseUnquote :: Parser LispVal
parseUnquote =
  do _ <- char ','
     e <- parseExpr
     return $ List [Atom "unquote", e]

parseVector :: Parser LispVal
parseVector =
  do try $ string "#("
     v <- sepBy parseExpr spaces
     char ')'
     (return . Vector . fromList) v

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
