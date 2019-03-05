module Main where

import Control.Monad

gcd :: Int -> Int -> Int
gcd a b
	| a < 0 || b < 0 = error "Qu'est-ce que c'est?"
	| otherwise = gcd' a b

	where	gcd' 0 b = b
		gcd' a 0 = a
		gcd' a b = gcd' b (a `mod` b)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b] <- liftM (map read . words) getLine :: IO [Int]
		putStrLn . show $ Main.gcd a b
