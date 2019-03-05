module Main where

import Control.Monad
import Data.List

main = do
	[_, shift] <- liftM (map read . words) getLine :: IO [Int]
	l <- liftM (map read . words) getLine :: IO [Integer]
	putStrLn . join $ intersperse " " . map show $ drop shift l ++ take shift l
