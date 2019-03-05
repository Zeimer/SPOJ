module Main where

import Control.Monad
import Data.Char

main = do
	s <- liftM (map words . lines) getContents :: IO [[String]]
	forM_ s $ \strs -> do
		let	nums = length . filter (\w -> not $ isAlpha (head w)) $ strs
			words = length . filter (\w -> isAlpha (head w)) $ strs
		putStrLn $ show nums ++ " " ++ show words
