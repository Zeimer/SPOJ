import Control.Monad

--main = liftM (unlines . map (show . (\[a, b] -> gcd a b) . map read . words) . lines) (getLine >> getContents) >>= putStr

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn . show $ gcd a b
