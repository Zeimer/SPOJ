module Main where

import Control.Monad

bigLCM :: [Integer] -> Integer
bigLCM [a, b] = lcm a b
bigLCM (n:ns) = lcm n (bigLCM ns)

test :: IO ()
test = do
	getLine
	l <- liftM (map read . words) getLine :: IO [Integer]
	putStrLn . show $ bigLCM l

main = do
	numOfTests <- liftM read getLine :: IO Int
	replicateM_ numOfTests test
