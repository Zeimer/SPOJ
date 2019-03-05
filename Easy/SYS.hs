import Control.Monad

binToBase :: Int -> Int -> String
binToBase base n = reverse . map digits $ aux n where
	aux :: Int -> [Int]
	aux n
		| n < base = [n]
		| otherwise =
			let	a = n `div` base
				b = n `mod` base
			in b : aux a

digits :: Int -> Char
digits n = case n of
	0 -> '0'
	1 -> '1'
	2 -> '2'
	3 -> '3'
	4 -> '4'
	5 -> '5'
	6 -> '6'
	7 -> '7'
	8 -> '8'
	9 -> '9'
	10 -> 'A'
	11 -> 'B'
	12 -> 'C'
	13 -> 'D'
	14 -> 'E'
	15 -> 'F'

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Int
		putStrLn $ binToBase 16 k ++ " " ++ binToBase 11 k
