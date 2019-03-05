module Main where

import Control.Monad
import Data.List

main = do
	l <- liftM (map read . words) getLine :: IO [Integer]
	putStrLn . join . reverse $ intersperse " " (map show l)
