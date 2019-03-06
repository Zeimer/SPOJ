module Main where

import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn $ if a `mod` b == 0 then "TAK" else "NIE"
