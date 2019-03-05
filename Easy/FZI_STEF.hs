module Main where

import Control.Monad

count :: [Integer] -> Integer
count l = aux 0 l where
	aux acc [] = acc
	aux acc (x:xs)
		| x < 0 = max acc (max (aux (acc + x) xs) (aux 0 xs))
		| otherwise = aux (acc + x) xs

main = getLine >> liftM (show . count . map read . lines) getContents >>= putStrLn
