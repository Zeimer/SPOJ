module Main where

import Control.Monad

merge :: String -> String -> String
merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) = x : y : merge xs ys

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[x, y] <- liftM words getLine :: IO [String]
		putStrLn $ merge x y
