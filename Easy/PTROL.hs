module Main where

import Control.Monad
import Data.List

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		_ : h : t <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn . join $ intersperse " " . map show $ t ++ [h]
