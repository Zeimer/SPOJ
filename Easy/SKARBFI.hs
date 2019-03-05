module Main where

import Control.Monad

findTreasure :: IO ()
findTreasure = do
	n <- liftM read getLine :: IO Int
	l <- replicateM n $ do
		[dir, k] <- liftM (map read . words) getLine :: IO [Integer]
		case dir of
			0 -> return (0, k)
			1 -> return (0, -k)
			2 -> return (-k, 0)
			3 -> return (k, 0)
	let location = foldr (\(a, b) (a', b') -> (a + a', b + b')) (0, 0) l
	case location of
		(0, 0) -> putStrLn "studnia"
		(x, y) -> do
			when (y /= 0) $ putStrLn $ (if y < 0 then "1 " else "0 ") ++ show (abs y)
			when (x /= 0) $ putStrLn $ (if x < 0 then "2 " else "3 ") ++ show (abs x)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n findTreasure
