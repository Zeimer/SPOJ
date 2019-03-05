module Main where

import Control.Monad
import Data.Set

primes :: Int -> [Int]
primes n = sieve [2..n] where
	sieve [] = []
	sieve (p:ns) = p : sieve (Prelude.filter (\n -> n `mod` p /= 0) ns)

main = do
	let mapOfPrimes = fromList $ primes 10000
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Int
		putStrLn $ if k `member` mapOfPrimes then "TAK" else "NIE"
