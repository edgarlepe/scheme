{-# LANGUAGE FlexibleContexts #-}

module ParserSpec
  ( unitTests
  )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Hspec as HS
import Test.Tasty.HUnit as HU

import Parser
import Text.ParserCombinators.Parsec (parse)

parseTest p input =
  case parse p "" input of
    Left  _   -> "Error"
    Right val -> case val of
                   Atom _         -> "Atom"
                   List _         -> "List"
                   DottedList _ _ -> "Dotted List"
                   Vector _       -> "Vector"
                   Complex _      -> "Complex"
                   Integer _      -> "Integer"
                   Real _         -> "Real"
                   Rational _     -> "Rational"
                   String _       -> "String"
                   Char _         -> "Char"
                   Bool _         -> "Boolean"

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [testCase "parseVector returns vector on valid input" case_parseVector1]

case_parseVector1 :: Assertion
case_parseVector1 = parseTest parseVector "#(1 2 3 4)" @=? "Vector"
