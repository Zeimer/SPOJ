module Main where

import Control.Monad

findPalindrome :: Integer -> (Integer, Int)
findPalindrome n = if show n == reverse (show n)
	then (n, 0)
	else let (res, k) = findPalindrome (n + read (reverse (show n))) in (res, k + 1)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Integer
		putStrLn $ case findPalindrome k of (res, adds) -> show res ++ " " ++ show adds
		
