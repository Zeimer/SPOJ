module Main where

import Data.Char

transform :: String -> String
transform "" = ""
transform " " = ""
transform [x] = [x]
transform (' ':c:cs) = transform (toUpper c : cs)
transform (c:cs) = c : transform cs

--main = fmap transform getContents >>= putStr

main = do
	s <- getContents
	putStr $ transform s
