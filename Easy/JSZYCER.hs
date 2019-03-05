import Control.Monad
import Data.Char

cipher :: String -> String
cipher "" = ""
cipher (c:cs) = c' : cipher cs where
	c' = if isAlpha c then chr (65 + ((ord c - 65 + 3) `mod` 26)) else c

main = liftM (unlines . map cipher . lines) getContents >>= putStr
