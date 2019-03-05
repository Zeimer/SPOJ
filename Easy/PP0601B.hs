module Main where

import Control.Monad
import Data.List

showL :: (Show a) => [a] -> String
showL = join . intersperse " " . map show

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[n, x, y] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn . showL $ filter (\k -> k `mod` x == 0 && k `mod` y /= 0) [2..n-1]
