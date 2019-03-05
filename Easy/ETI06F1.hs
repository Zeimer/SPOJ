module Main where

import Control.Monad

calc :: Double -> Double -> Double
calc r d =
	let	r' = sqrt (r^2 - (d/2)^2)
	in	pi * r'^2

main = do
	[r, d] <- liftM (map read . words) getLine :: IO [Double]
	putStrLn . show $ calc r d
