module Main where

import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b] <- liftM (map read . words) getLine :: IO [Int]
		let c = [last . show $ a^b]
		putStrLn c
