module Sudoku where

import Data.Char
import Data.List

data Sudoku = Sudoku [[Maybe Int]]
            deriving (Show, Eq)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

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
--http://stackoverflow.com/a/31361604/3861396
blocks :: Sudoku  -> [Block]
blocks (Sudoku rs) =  (map concat . groupBy3 . concat . transpose . map groupBy3) rs 
  where
    groupBy3 :: [t] -> [[t]]
    groupBy3 (a:b:c:ds) = [a,b,c] : groupBy3 ds
    groupBy3 []         = []

--D3
isOkaySection :: [Maybe Int] -> Bool
isOkaySection s = vals == (nub vals)
  where vals = filter (/=Nothing) s

columns :: [[Maybe Int]] -> [[Maybe Int]]
columns rs 
  | (column . map (drop 1)) rs == [] = rs
  | otherwise = (column rs) : (columns . map (drop 1)) rs 

column :: [[Maybe Int]] -> [Maybe Int]
column = concatMap (take 1)

--No duplicates
isOkay :: Sudoku -> Bool
isOkay s@(Sudoku rs) = isOkayBlocks && isOkayRows && isOkayCols
  where
    isOkayBlocks = foldl (\acc currBlock -> acc && (isOkayBlock currBlock)) True (blocks s)
    isOkayRows = foldl (\acc currRow -> acc && (isOkaySection currRow)) True rs
    isOkayCols = foldl (\acc currCol -> acc && (isOkaySection currCol)) True (columns rs)

--E1
type Pos = (Int, Int)

nothings :: [[Maybe Int]] -> [Pos]
nothings rs = [(i, j) | i <- [0..length rs - 1], j <- [0..length (rs!!i) - 1], (rs!!i)!!j == Nothing]

hSort :: [[Maybe Int]] -> [Pos]
hSort rs = sortBy (\a b -> sortNothings rs a b) (nothings rs)

numNothings :: [Maybe Int] -> Int
numNothings = foldl (\acc curr -> if curr == Nothing then acc+1 else acc) 0 

sortNothings :: [[Maybe Int]] -> Pos -> Pos -> Ordering
sortNothings rs (r1, c1) (r2, c2) 
	| h1 < h2 = LT 
	| h1 > h2 = GT
	| otherwise=compare h3 h4
	where
		h1 = numNothings (rs !! r1)
		h2 = numNothings (rs !! r2)
		h3 = numNothings ((columns rs) !! c1)
		h4 = numNothings ((columns rs) !! c2)

blank :: Sudoku -> Pos
blank sud@(Sudoku rs) = head $ hSort rs
--blank (Sudoku rs) = head $ nothings rs

--E2
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (i, v) = [if (j==i) then v else xs!!j | j <- [0..length xs - 1]]

--E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rs) (i, j) v = Sudoku [if (ii == i) then (rs!!ii) !!= (j, v) else rs!!ii | ii <- [0..length rs - 1]]

--F1
solve :: Sudoku -> Maybe Sudoku
solve sud@(Sudoku rs) 
  | not (isOkay sud) || not (isSudoku sud) = Nothing
  | isSolved sud = Just sud
  | otherwise = if solutions == [] then Nothing else head solutions 
    where solutions = filter (/= Nothing) [solve (update sud (blank sud) (Just v)) | v <- [1..9]] 