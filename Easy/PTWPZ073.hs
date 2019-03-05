import Control.Monad
import Data.Char

charToNum :: Char -> Int
charToNum c
	| ord c < 65 = error "WUT"
	| 65 <= ord c && ord c <= 82 = 2 + (ord c - 65) `div` 3
	| ord c == 83 = 7
	| 84 <= ord c && ord c <= 86 = 8
	| 87 <= ord c && ord c <= 90 = 9
	| otherwise = error "WUT"

solveOne :: String -> String
solveOne = join . map (show . charToNum)

main = liftM (tail . lines) getContents >>= mapM_ (putStrLn . solveOne)
