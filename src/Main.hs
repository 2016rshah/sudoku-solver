module Main where

import Sudoku
import Data.Maybe

main = do
  examples
  --mediums
  --hards

readAndSolve :: FilePath -> IO ()
readAndSolve s = do
  sud <- readSudoku s
  printSudoku (fromJust (solve sud))

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

hards :: IO ()
hards = do
  putStrLn "\nHard example"
  readAndSolve "src/boards/hards/ex2.sud"

example :: Sudoku
example =
  Sudoku
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