module Main where

import Control.Monad

main = do
	l <- fmap (map read . lines) getContents :: IO [Integer]
	mapM (putStrLn . show) $ tail (scanl (+) 0 l)
