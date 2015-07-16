module Main where

import Sudoku

import Data.Maybe
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of 
    (c:cs) -> case c of
      "examples" -> examples
      "mediums" -> mediums
      "hards" -> hards
      "euler" -> euler
      _ -> mediums
    _ -> mediums

readAndSolve :: FilePath -> IO ()
readAndSolve s = do
  sud <- readSudoku s
  let solved = solve sud
  if solved == Nothing then putStrLn "Nothing" else printSudoku (fromJust solved)

euler :: IO ()
euler = do 
  putStrLn "\nhttps://projecteuler.net/project/resources/p096_sudoku.txt"
  readAndSolve "src/boards/euler/01.sud"
  putStrLn ""
  readAndSolve "src/boards/euler/50.sud"

examples :: IO ()
examples  = do
  putStrLn "\nExample solve from hard coded board"
  printSudoku (fromJust (solve example))
  putStrLn "\nExample solve reading from file"
  readAndSolve "src/boards/examples/example.sud"
  putStrLn "\nImpossible board to solve"
  readAndSolve "src/boards/examples/impossible.sud"

mediums :: IO ()
mediums = do 
  putStrLn "\nMedium examples\n"
  readAndSolve "src/boards/mediums/ex2.sud"
  putStrLn ""
  readAndSolve "src/boards/mediums/ex3.sud"
  putStrLn ""
  readAndSolve "src/boards/mediums/ex4.sud"
  putStrLn ""
  readAndSolve "src/boards/mediums/ex5.sud"

hards :: IO ()
hards = do
  putStrLn "\nHard example"
  --readAndSolve "src/boards/hards/ex2.sud"
  readAndSolve "src/boards/hards/ex13.sud"

example :: Sudoku
example =
  [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
  , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
  , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
  , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
  , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
  , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
  , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
  , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
  , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
  ]