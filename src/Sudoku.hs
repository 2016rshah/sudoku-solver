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
    correctLen :: [a] -> Bool
    correctLen = (9 ==) . length

    lenColsWorks :: [[a]] -> Bool
    lenColsWorks = all (== True) . map correctLen

    digitsInRange :: [[Maybe Int]] -> Bool
    digitsInRange = all (== True) . map helper 
      where
        helper :: [Maybe Int] -> Bool
        helper = all (== True) . map (\x -> x `elem` Nothing:[Just x | x <- [1..9]]) 

--A3
isSolved :: Sudoku -> Bool
isSolved (Sudoku rs) = (all (== True) . map (not . elem Nothing)) rs 

--B1
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = (putStr . stringifySudoku) rs

stringifySudoku :: [[Maybe Int]]  -> String
stringifySudoku = concatMap printRow 


printRow :: [Maybe Int] -> String
printRow row = (map printCell row) ++ "\n"

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
parseSudoku = (Sudoku . map lineToRow . lines)

lineToRow :: String -> [Maybe Int]
lineToRow = map charToCell

charToCell :: Char -> Maybe Int
charToCell '.' = Nothing
charToCell c = Just (digitToInt c)

--Skipping C because I don't want to do that. I'll come back to it if I need it

--D2
--http://stackoverflow.com/a/31361604/3861396
blocks :: [[Maybe Int]]  -> [[Maybe Int]]
blocks =  (map concat . groupBy3 . concat . transpose . map groupBy3)
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
isOkay s@(Sudoku rs) = (g rs) && ((g . blocks) rs) && ((g . columns) rs)
  where
    g = all (== True) . map isOkaySection 


--E1 and X
type Pos = (Int, Int)

nothings :: [[Maybe Int]] -> [Pos]
nothings rs = [(i, j) | i <- [0..length rs - 1], j <- [0..length (rs!!i) - 1], (rs!!i)!!j == Nothing]

hSort :: [[Maybe Int]] -> [Pos]
hSort rs = sortBy (\a b -> sortNothings rs a b) (nothings rs)

numNothings :: [Maybe Int] -> Int
numNothings = foldl f 0 
  where 
    f acc curr 
      | curr == Nothing = acc+1
      | otherwise = acc

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

--Y
--pRows :: Sudoku -> Sudoku 
--pRows (Sudoku rs) = (filter (==1) . map numNothings) rs

--propogate :: Sudoku -> Sudoku
--propogate = pRows

--F1
solve :: Sudoku -> Maybe Sudoku
solve sud@(Sudoku rs) 
  | not (isOkay sud) || not (isSudoku sud) = Nothing
  | isSolved sud = Just sud
  | otherwise = if solutions == [] then Nothing else head solutions 
    where solutions = filter (/= Nothing) [solve (update sud (blank sud) (Just v)) | v <- [1..9]] 