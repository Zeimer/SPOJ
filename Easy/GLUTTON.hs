module Main where

import Control.Monad

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[numOfParticipants, candiesPerBox] <- liftM (map read . words) getLine :: IO [Int]
		strTimes <- replicateM numOfParticipants getLine :: IO [String]
		let	times = map read strTimes :: [Int]
			numOfCakes = sum $ map (\t -> 24 * 60 * 60 `div` t) times
		putStrLn . show $ ceiling (fromIntegral numOfCakes / fromIntegral candiesPerBox)
