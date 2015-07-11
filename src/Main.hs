module Main where

import Data.Char
import Data.List

main = do 
  s <- readSudoku "src/board.txt"
  putStrLn "Showing the input sudoku"
  printSudoku s


data Sudoku = Sudoku [[Maybe Int]]
            deriving Show

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

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

--A1
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (take 9 (repeat row))
  where row = take 9 (repeat Nothing)

--A2
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = correctLen rs && lenColsWorks rs && digitsInRange rs
  where
    allTrue :: [Bool] -> Bool
    allTrue = foldl (\acc curr -> acc && curr) True

    correctLen :: [a] -> Bool
    correctLen rs = (length rs) == 9

    lenColsWorks :: [[a]] -> Bool
    lenColsWorks = allTrue . map correctLen

    digitsInRange :: [[Maybe Int]] -> Bool
    digitsInRange = allTrue . map helper 
      where
        helper :: [Maybe Int] -> Bool
        helper = allTrue . map (\x -> x `elem` Nothing:[Just x | x <- [1..9]]) 

--A3
isSolved :: Sudoku -> Bool
isSolved (Sudoku rs) = foldl (\acc curr -> acc && not (Nothing `elem` curr)) True rs  

--B1
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = (putStr . stringifySudoku) rs

stringifySudoku :: [[Maybe Int]]  -> String
stringifySudoku = foldl (\acc curr -> acc ++ printRow curr) "" 

printRow :: [Maybe Int] -> String
printRow row = reverse $ '\n' : (foldl (\acc curr -> (printCell curr):acc) "" row)

printCell :: Maybe Int -> Char
printCell Nothing = '.'
printCell (Just x)= intToDigit x

--B2
-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
  f <- readFile file
  return (parseSudoku f)

parseSudoku :: String -> Sudoku
parseSudoku = (Sudoku . reverse . foldl (\acc curr -> (lineToRow curr):acc) [] . lines)

lineToRow :: String -> [Maybe Int]
lineToRow = reverse . foldl (\acc curr -> (charToCell curr):acc) []  

charToCell :: Char -> Maybe Int
charToCell '.' = Nothing
charToCell c = Just (digitToInt c)

--Skipping C because I don't want to do that. I'll come back to it if I need it

type Block = [Maybe Int]

--D1
isOkayBlock :: Block -> Bool
isOkayBlock b = vals == (nub vals)
  where vals = filter (/=Nothing) b

--D2
--blocks :: Sudoku -> [Block]
--blocks (Sudoku rs) = block1 : block2 : block3 : block4 : block5 : block6 : block7 : block8 : block9 : []
--  where block1 = [(rs!!0)!!0] ++ [(rs!!0)!!1] ++ [(rs!!0)!!2] ++ [(rs!!1)!!0] ++ [(rs!!1)!!1] ++ [(rs!!1)!!2]++ [(rs!!2)!!0] ++ [(rs!!2)!!1] ++ [(rs!!2)!!2]  
--        block2 = 