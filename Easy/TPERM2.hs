module Main where

import Control.Monad
import Data.List

perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms l = do
	h <- l
	t <- perms (delete h l)
	return $ h : t

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Int
		mapM putStrLn (sort . perms $ take k ['a'..])
