module Main where

import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn . show $ 2 * gcd a b
