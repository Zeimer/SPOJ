module Main where

import Control.Monad

primes :: Int -> [Int]
primes n = sieve [2..n] where
	sieve [] = []
	sieve (p:ns) = p : sieve (filter (\n -> n `mod` p /= 0) ns)

main = do
	let p = primes 10000
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Int
		putStrLn $ if k `elem` p then "TAK" else "NIE"
