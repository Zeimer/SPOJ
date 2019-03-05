import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as BS

data Answer = No | Yes [Int]

instance Show Answer where
	show No = "NIE"
	show (Yes l) = join . intersperse " " . map show $ l

findAnswer' :: Int -> Answer
findAnswer' n
	| n < 0 = No
	| n == 0 = Yes [0]
	| n == 1 || n == 2 = No
	| n == 3 = Yes [1, 3, 0, 2]
	| otherwise = Yes $ [0,2..n] ++ [1,3..n]

solve :: Int -> BS.ByteString 
solve = BS.pack . show . findAnswer'

main = getLine >>= BS.putStrLn . solve . read
