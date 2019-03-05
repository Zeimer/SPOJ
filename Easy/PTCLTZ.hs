module Main where

import Control.Monad

collatz :: Integer -> [Integer]
collatz s
	| s == 1 = [1]
	| s `mod` 2 == 0 = s : collatz (s `div` 2)
	| otherwise = s : collatz (3 * s + 1)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine
		putStrLn . show . fst . head . filter (\(_, x) -> x == 1) $ zip [0..] (collatz k)
