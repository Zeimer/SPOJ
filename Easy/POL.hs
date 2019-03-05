module Main where

import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		s <- getLine
		putStrLn $ take (length s `div` 2) s
