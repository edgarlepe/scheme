module Main where

import Lib
import System.Environment

main :: IO ()
main =
  do args <- getArgs
     case args of
       (x:_) -> putStrLn $ readExpr x
       _     -> putStrLn "No argument"
