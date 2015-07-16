module Sudoku where

import Data.Char
import Data.List

type Block = [Maybe Int]
type Sudoku = [Block]

rows :: Sudoku -> [[Maybe Int]]
rows rs = rs

--A1
allBlankSudoku :: Sudoku
allBlankSudoku = (take 9 (repeat row))
  where row = take 9 (repeat Nothing)

--A2
isSudoku :: Sudoku -> Bool
isSudoku rs = correctLen rs && lenColsWorks rs && digitsInRange rs
  where
    correctLen :: [a] -> Bool
    correctLen = (9 ==) . length

    lenColsWorks :: [[a]] -> Bool
    lenColsWorks = all (== True) . map correctLen

    digitsInRange :: Sudoku -> Bool
    digitsInRange = all (== True) . map helper 
      where
        helper :: Block -> Bool
        helper = all (== True) . map (\x -> x `elem` Nothing:[Just x | x <- [1..9]]) 

--A3
isSolved :: Sudoku -> Bool
isSolved rs = (all (== True) . map (not . elem Nothing)) rs 

--B1
printSudoku :: Sudoku -> IO ()
printSudoku rs = (putStr . stringifySudoku) rs

stringifySudoku :: Sudoku  -> String
stringifySudoku = concatMap printRow 


printRow :: Block -> String
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
parseSudoku = (map lineToRow . lines)

lineToRow :: String -> Block
lineToRow = map charToCell

charToCell :: Char -> Maybe Int
charToCell '.' = Nothing
charToCell c = Just (digitToInt c)

--D2
--http://stackoverflow.com/a/31361604/3861396
blocks :: Sudoku  -> [Block]
blocks =  (map concat . groupBy3 . concat . transpose . map groupBy3)
  where
    groupBy3 :: [t] -> [[t]]
    groupBy3 (a:b:c:ds) = [a,b,c] : groupBy3 ds
    groupBy3 []         = []

--D3
isOkaySection :: Block -> Bool
isOkaySection s = vals == (nub vals)
  where vals = filter (/=Nothing) s

columns :: Sudoku -> Sudoku
columns rs 
  | (column . map (drop 1)) rs == [] = rs
  | otherwise = (column rs) : (columns . map (drop 1)) rs 

column :: Sudoku -> Block
column = concatMap (take 1)

--No duplicates
isOkay :: Sudoku -> Bool
isOkay rs = (g rs) && ((g . blocks) rs) && ((g . columns) rs)
  where
    g = all (== True) . map isOkaySection 


--E1 and X
type Pos = (Int, Int)

nothings :: Sudoku -> [Pos]
nothings rs = [(i, j) | i <- [0..length rs - 1], j <- [0..length (rs!!i) - 1], (rs!!i)!!j == Nothing]

hSort :: Sudoku -> [Pos]
hSort rs = sortBy (\a b -> sortNothings rs a b) (nothings rs)

numNothings :: Block -> Int
numNothings = foldl f 0 
  where 
    f acc curr 
      | curr == Nothing = acc+1
      | otherwise = acc

sortNothings :: Sudoku -> Pos -> Pos -> Ordering
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
blank rs = head $ hSort rs
--blank (Sudoku rs) = head $ nothings rs

--E2
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (i, v) = [if (j==i) then v else xs!!j | j <- [0..length xs - 1]]

--E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update rs (i, j) v = [if (ii == i) then (rs!!ii) !!= (j, v) else rs!!ii | ii <- [0..length rs - 1]]

--Y
--pRows :: Sudoku -> Sudoku 
--pRows (Sudoku rs) = (filter (==1) . map numNothings) rs

--propogate :: Sudoku -> Sudoku
--propogate = pRows

--F1
solve :: Sudoku -> Maybe Sudoku
solve sud
  | not (isOkay sud) || not (isSudoku sud) = Nothing
  | isSolved sud = Just sud
  | otherwise = if solutions == [] then Nothing else head solutions 
    where solutions = filter (/= Nothing) [solve (update sud (blank sud) (Just v)) | v <- [1..9]] 