module Main where

import Control.Monad

testDivisor :: Integer -> Integer -> Bool
testDivisor a b = a `mod` b == 0

boolToAns :: Bool -> String
boolToAns True = "TAK"
boolToAns False = "NIE"

{-main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn . boolToAns $ testDivisor a b-}

main = do
	n <- liftM read getLine :: IO Int
	str <- getContents
	forM_ (lines str) $ \line -> do
		let [a, b] = map read . words $ line
		putStrLn . boolToAns $ testDivisor a b
