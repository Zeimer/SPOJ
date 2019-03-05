module Main where

import Control.Monad

main = do
	l <- liftM (map words . lines) getContents :: IO [[String]]
	forM_ l $ \[s1, op, s2] -> do
		let	a = read s1 :: Integer
			b = read s2 :: Integer
		putStrLn . (\b -> if b then "1" else "0") $ case op of
			"==" -> a == b
			"!=" -> a /= b
			">=" -> a >= b
			"<=" -> a <= b
