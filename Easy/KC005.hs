import Control.Monad
import Data.Char

isValidName :: String -> Bool
isValidName [] = False
isValidName str = isUpper (head str) && all isAlpha str && all isLower (tail str)

isValidSurname :: String -> Bool
isValidSurname = isValidName

type Year = Int
type Month = Int
type Day = Int

data Date = Bad | Date Year Month Day

instance Show Date where
	show Bad = "Bad"
	show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

isValidYear :: Int -> Bool
isValidYear y = 1900 <= y && y <= 2000

isValidMonth :: Int -> Bool
isValidMonth m = 1 <= m && m <= 12

isValidDay :: Int -> Bool
isValidDay d = 1 <= d && d <= 31

isValidDate :: Date -> Bool
isValidDate Bad = False
isValidDate (Date y m d) = isValidYear y && isValidMonth m && isValidDay d

isValidLine :: (String, String, Date) -> Int
isValidLine (name, surname, date)
	| not (isValidName name) = 0
	| not (isValidSurname surname) = 1
	| not (isValidDate date) = 2
	| otherwise = 3

parseDate :: String -> Date
parseDate (r1:r2:r3:r4:_:m1:m2:_:d1:d2:[])
	| all isDigit [r1, r2, r3, r4, m1, m2, d1, d2] = Date (read [r1, r2, r3, r4]) (read [m1, m2]) (read [d1, d2])
	| otherwise = Bad
parseDate _ = Bad

parseLine :: String -> (String, String, Date)
parseLine str =
	let	noSemicolons = filter (/= ';') str
		w = words noSemicolons
		name = w !! 1
		surname = w !! 3
		date = parseDate $ w !! 6
	in (name, surname, date)

--main = liftM (map (isValidLine . parseLine) . lines) getContents >>= mapM_ (putStrLn . show)
{-main = do
	str <- getContents
	forM_ (lines str) $ \line -> do
		putStrLn . show . isValidLine . parseLine $ line-}
main = do
	str <- getContents
	forM_ (lines str) (putStrLn . show . isValidLine . parseLine)
