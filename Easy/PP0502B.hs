module Main where

import Control.Monad
import Data.List

strToList :: String -> [Int]
strToList = map read . words

listToStr :: [Int] -> String
listToStr = concat . intersperse " " . map show

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		l <- liftM (drop 1 . strToList) getLine :: IO [Int]
		putStrLn . listToStr . reverse $ l
