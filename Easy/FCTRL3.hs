module Main where

import Control.Monad

fac :: Int -> Int
fac n = product [1..n]

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Int
		if k <= 9
		then do
			let [dec, one] = reverse . take 2 . reverse $ "0" ++ show (fac k)
			putStrLn $ [dec] ++ " " ++ [one]
		else do
			putStrLn "0 0"
