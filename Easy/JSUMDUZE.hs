module Main where

import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		s <- liftM (sum . map read . words) getLine :: IO Integer
		putStrLn $ show s
