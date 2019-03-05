import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	numbers <- liftM (map read) (replicateM n getLine) :: IO [Integer]
	op:_:rest <- getLine
	let	x = read rest :: Integer
		p = case op of
			'<' -> (< x)
			'>' -> (> x)
			_ -> error "ERROR"
	mapM_ (putStrLn . show) $ filter p numbers
