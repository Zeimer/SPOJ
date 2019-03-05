module Main where

import Control.Monad
import qualified Data.Map as Map
import Data.Char
import Data.List

count :: String -> [(Char, Int)]
count s =
	let	s' = filter isAlpha s
		lowerMap = Map.fromList $ zip ['a'..'z'] [0,0..]
		upperMap = Map.fromList $ zip ['A'..'Z'] [0,0..]
		lower = foldr (Map.update (\n -> Just (n + 1))) lowerMap s'
		upper = foldr (Map.update (\n -> Just (n + 1))) upperMap s'
	in filter (\(_, n) -> n /= 0) $ sort (Map.toList lower) ++ sort (Map.toList upper)

main = do
	getLine
	s <- getContents
	forM_ (count s) $ \(c, n) -> do
		putStrLn $ c : " " ++ show n
