module Main where

import Control.Monad

validate :: [Int] -> Bool
validate pesel =
	let s = sum $ zipWith (*) [1, 3, 7, 9, 1, 3, 7, 9, 1, 3, 1] pesel in
	if s <= 0
	then False
	else last (show s) == '0'

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		pesel <- liftM (map (read . return)) getLine :: IO [Int]
		putStrLn $ if validate pesel then "D" else "N"
