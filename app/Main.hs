module Main where

import Parser.Parser
import Eval.Eval

import System.Environment

main :: IO ()
main = do
  pro <- readFile "program.txt"
  inp <- readFile "input.txt"
  case runFromFile pro  "program.txt" (words inp) of
    Left err -> print err
    Right out -> let content = toOutput out in
      putStrLn content >> writeFile "output.txt" content
  return ()


runFromFile pro file input = do
  ast <- getAST file pro
  runProgram ast input

toOutput :: [String] -> String
toOutput = unlines . reverse
