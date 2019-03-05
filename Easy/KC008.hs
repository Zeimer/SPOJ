module Main where

import Control.Monad

main = do
	sums <- liftM (map (sum . map read . words) . lines) getContents :: IO [Integer]
	mapM (putStrLn . show) sums
	putStrLn . show . sum $ sums
