module Main where

import Control.Monad
import Data.List

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		_ : l <- liftM (map read . words) getLine :: IO [Double]
		let	mean = sum l / fromIntegral (length l)
			sorted = sortBy (\x y -> compare (abs $ mean - x) (abs $ mean - y)) l
		putStrLn . show . floor $ head sorted
