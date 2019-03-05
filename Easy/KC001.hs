module Main where

import Control.Monad

main = do
	s <- liftM (sum . map read) $ replicateM 3 getLine :: IO Integer
	putStrLn $ show s
