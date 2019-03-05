module Main where

import Control.Monad

main = liftM (f . map read . words . head . lines) getContents >>= putStrLn . show
	where	f :: [Integer] -> Integer
		f [a, b, c, d] = a * b + c * d
