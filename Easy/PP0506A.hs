module Main where

import Control.Monad
import Data.List

dist :: Integer -> Integer -> Double
dist x y =
	let	x' = fromIntegral x
		y' = fromIntegral y
	in sqrt (x'^2 + y'^2)

showPoint :: (String, Integer, Integer) -> String
showPoint (s, x, y) = s ++ " " ++ show x ++ " " ++ show y

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Int
		points <- replicateM k $ do
			[label, x, y] <- liftM words getLine
			return (label, read x, read y) :: IO (String, Integer, Integer)

		let	points' = sortBy (\(_, x, y) (_, x', y') -> compare (dist x y) (dist x' y')) points
		mapM_ (putStrLn . showPoint) points'
		getLine
			
