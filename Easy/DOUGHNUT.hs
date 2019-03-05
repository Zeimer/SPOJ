import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[c, k, w] <- liftM (map read . words) getLine :: IO [Int]
		putStrLn $ if c * w <= k then "yes" else "no"
