module Main where

import Control.Monad
import Data.List

readMatr :: (Read a) => Int -> IO [[a]]
readMatr 0 = return []
readMatr rows = do
	r <- liftM (map read . words) getLine
	rs <- readMatr (rows - 1)
	return $ r : rs

myShowList :: (Show a) => [a] -> String
myShowList = join . intersperse " " . map show

showMatr :: (Show a) => [[a]] -> IO ()
showMatr = mapM_ (putStrLn . myShowList)

myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose l = map head l : myTranspose (map tail l)

main = do
	[rows, _] <- liftM (map read . words) getLine :: IO [Int]
	m <- readMatr rows :: IO [[Integer]]
	showMatr (transpose m)
	
