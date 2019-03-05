import Control.Monad
import Data.List

data Sudoku = Sudoku [[Int]]

readL :: (Read a) => String -> [a]
readL = map read . words

instance Read Sudoku where
	readsPrec _ str =
		let	rows = map readL $ lines str
			numOfRows = length rows
			badRows = any (\row -> length row /= 9) rows

		in	if numOfRows /= 9 || badRows
			then error $ "numOfRows = " ++ (show numOfRows) ++ ", badRows = " ++ (show badRows)
	 		else [(Sudoku rows, "")]

showL :: (Show a) => [a] -> String
showL = join . intersperse " " . map show

instance Show Sudoku where
	show (Sudoku x) = unlines $ map showL x

verifyRows :: Sudoku -> Bool
verifyRows (Sudoku rows) = nub (map sort rows) == [[1..9]]

verifyCols :: Sudoku -> Bool
verifyCols (Sudoku rows) = verifyRows (Sudoku $ transpose rows)

verifySquare :: Sudoku -> (Int, Int) -> Bool
verifySquare (Sudoku rows) (i, j) =
	let	smallRows = take 3 $ drop (3 * i) rows
		smallCols = map (take 3 . drop (3 * j)) smallRows
	in sort (join smallCols) == [1..9]

verifySquares :: Sudoku -> Bool
verifySquares s =
	let	indices = [(i, j) | i <- [0..2], j <- [0..2]]
	in all (verifySquare s) indices

verify :: Sudoku -> Bool
verify s = verifyRows s && verifyCols s && verifySquares s

run :: (Sudoku -> Bool) -> IO ()
run verify = do
	sudoku <- liftM (read . unlines) (replicateM 9 getLine) :: IO Sudoku
	putStrLn $ if verify sudoku then "TAK" else "NIE"

main = do
	n <- liftM read getLine :: IO Int
	sequence_ . intersperse (getLine >> return ()) $ replicate n (run verify)
