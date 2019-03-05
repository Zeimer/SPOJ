module Main where

import Control.Monad
import Data.List

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		_ : l <- liftM (map read . words) getLine :: IO [Integer]
		let	(odd, even) = split l
			o = join $ intersperse " " (map show odd)
			e = join $ intersperse " " (map show even)
		putStrLn $ e ++ (' ' : o)
