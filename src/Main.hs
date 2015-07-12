module Main where

import Sudoku
import Data.Maybe

main = do
  --examples
  mediums
  --hards

examples :: IO ()
examples  = do
  putStrLn "\nExample solve. Should return solved board"
  readAndSolve "src/boards/examples/example.sud"
  putStrLn "\nImpossible board to solve"
  readAndSolve "src/examples/impossible.sud"

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

readAndSolve :: FilePath -> IO ()
readAndSolve s = do
  sud <- readSudoku s
  printSudoku (fromJust (solve sud))
