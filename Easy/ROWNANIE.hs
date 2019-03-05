module Main where

import Control.Monad

delta :: Double -> Double -> Double -> Double
delta a b c = b^2 - 4 * a * c

numOfSolutions :: Double -> Int
numOfSolutions delta
	| delta < 0 = 0
	| delta == 0 = 1
	| otherwise = 2

main = do
	lines <- liftM lines getContents :: IO [String]
	forM_ lines $ \line -> do
		let [a, b, c] = map read (words line) :: [Double]
		putStrLn . show . numOfSolutions $ delta a b c
