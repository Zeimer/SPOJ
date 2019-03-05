module Main where

import Control.Monad

fac :: Integer -> Integer
fac n = product [1..n]

binom :: Integer -> Integer -> Integer
binom n k = fac n `div` (fac k * fac (n - k))

main = do
	numOfLines <- liftM read getLine :: IO Int
	replicateM_ numOfLines $ do
		[n, k] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn . show $ binom n k
