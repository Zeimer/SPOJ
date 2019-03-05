module Main where

import Control.Monad

single :: IO ()
single = do
	getLine
	s <- liftM (sum . map read . words) getLine :: IO Integer
	putStrLn . show $ s

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n single
