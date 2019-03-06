import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	l <- replicateM n (fmap read getLine) :: IO [Int]
	
	getLine

	k <- liftM read getLine :: IO Int
	l' <- replicateM k (fmap read getLine) :: IO [Int]

	let solution = map (\k -> length (filter (< k) l)) l'

	mapM_ (putStrLn . show) solution
