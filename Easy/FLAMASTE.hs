module Main where

import Control.Monad

transform :: String -> String
transform [] = []
transform (c:cs) = c : middle ++ rest
	where	p = \c' -> c' == c
		len = length $ takeWhile p (c : cs)
		middle = case len of
			1 -> ""
			2 -> [c]
			_ -> show len
		rest = transform (dropWhile p cs)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		s <- getLine
		putStrLn . transform $ s
