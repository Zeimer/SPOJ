module Main where

import Control.Monad

process :: Int -> [Int] -> [Int]
process _ [] = []
process _ [x] = [x]
process 3 _ = []
process n (x:42:xs) = if x == 42 then x : process n (42:xs) else x : 42 : process (n + 1) xs
process n (x:y:zs) = x : process n (y:zs)

main = do
	l <- liftM (map read . lines) getContents :: IO [Int]
	mapM_ (putStrLn . show) $ process 0 l
