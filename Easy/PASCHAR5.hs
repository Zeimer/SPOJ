import Control.Monad
import Data.Char

cipher :: String -> String
cipher "" = ""
cipher (c:cs)
	| isDigit c = chr (48 + ((ord c - 48 + 5) `mod` 10)) : cipher cs
	| isUpper c = chr (65 + ((ord c - 65 + 13) `mod` 26)) : cipher cs
	| isLower c = chr (97 + ((ord c - 97 + 13) `mod` 26)) : cipher cs
	| otherwise = c : cipher cs

main = liftM (unlines . map cipher . lines) getContents >>= putStr
