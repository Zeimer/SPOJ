import Control.Monad

numToWord' :: Int -> String
numToWord' n
	| n == 0 = "zero"
	| otherwise = numToWord n

numToWord :: Int -> String
numToWord n
	| n == 0 = ""
	| n == 1 = "jeden"
	| n == 2 = "dwa"
	| n == 3 = "trzy"
	| n == 4 = "cztery"
	| n == 5 = "pięć"
	| n == 6 = "sześć"
	| n == 7 = "siedem"
	| n == 8 = "osiem"
	| n == 9 = "dziewięć"
	| n == 10 = "dziesięć"
	| n == 11 = "jedenaście"
	| n == 12 = "dwanaście"
	| n == 13 = "trzynaście"
	| n == 14 = "czternaście"
	| n == 15 = "piętnaście"
	| n == 16 = "szesnaście"
	| n == 17 = "siedemnaście"
	| n == 18 = "osiemnaście"
	| n == 19 = "dziewiętnaście"
	| n == 20 = "dwadzieścia"
	| n < 30 = numToWord 20 ++ " " ++ numToWord (n - 20)
	| n == 30 = "trzydzieści"
	| n < 40 = numToWord 30 ++ " " ++ numToWord (n - 30)
	| n == 40 = "czterdzieści"
	| n < 50 = numToWord 40 ++ " " ++ numToWord (n - 40)
	| n == 50 = "pięćdziesiąt"
	| n < 60 = numToWord 50 ++ " " ++ numToWord (n - 50)
	| n == 60 = "sześćdziesiąt"
	| n < 70 = numToWord 60 ++ " " ++ numToWord (n - 60)
	| n == 70 = "siedemdziesiąt"
	| n < 80 = numToWord 70 ++ " " ++ numToWord (n - 70)
	| n == 80 = "osiemdziesiąt"
	| n < 90 = numToWord 80 ++ " " ++ numToWord (n - 80)
	| n == 90 = "dziewięćdziesiąt"
	| n < 100 = numToWord 90 ++ " " ++ numToWord (n - 90)
	| n == 100 = "sto"
	| n < 200 = numToWord 100 ++ " " ++ numToWord (n - 100)
	| n == 200 = "dwieście"
	| n < 300 = numToWord 200 ++ " " ++ numToWord (n - 200)
	| n == 300 = "trzysta"
	| n < 400 = numToWord 300 ++ " " ++ numToWord (n - 300)
	| n == 400 = "czterysta"
	| n < 500 = numToWord 400 ++ " " ++ numToWord (n - 400)
	| n == 500 = "pięćset"
	| n < 600 = numToWord 500 ++ " " ++ numToWord (n - 500)
	| n == 600 = "sześćset"
	| n < 700 = numToWord 600 ++ " " ++ numToWord (n - 600)
	| n == 700 = "siedemset"
	| n < 800 = numToWord 700 ++ " " ++ numToWord (n - 700)
	| n == 800 = "osiemset"
	| n < 900 = numToWord 800 ++ " " ++ numToWord (n - 800)
	| n == 900 = "dziewięćset"
	| n < 10^3 = numToWord 900 ++ " " ++ numToWord (n - 900)
	| n `elem` [1000, 2000 .. 9000] = numToWord (n `div` 1000) ++ " " ++ conjugK (n `div` 1000)
	| n < 10^6 = numToWord (n `div` 1000) ++ " " ++ conjugK (n `div` 1000) ++ " " ++ numToWord (n `mod` 1000)
	| n `elem` [10^6, 2 * 10^6 .. 9 * 10^6] = numToWord (n `div` 10^6) ++ " " ++ conjugM (n `div` 10^6)
	| n < 10^9 = numToWord (n `div` 10^6) ++ " " ++ conjugM (n `div` 10^6) ++ " " ++ numToWord (n `mod` 10^6)
	| n `elem` [10^9, 2 * 10^9 .. 9 * 10^9] = numToWord (n `div` 10^9) ++ " " ++ conjugMM (n `div` 10^9)
	| n < 10^12 = numToWord (n `div` 10^9) ++ " " ++ conjugMM (n `div` 10^9) ++ " " ++ numToWord (n `mod` 10^9)
	| n == 10^12 = "jeden bilion"

conjugK :: Int -> String
conjugK n
	| n `mod` 1000 == 1 = "tysiąc"
	| n `mod` 1000 <= 4 = "tysiące"
	| otherwise = "tysięcy"

conjugM :: Int -> String
conjugM n
	| n `mod` 1000 == 1 = "milion"
	| n `mod` 1000 <= 4 = "miliony"
	| otherwise = "milionów"

conjugMM :: Int -> String
conjugMM n
	| n `mod` 1000 == 1 = "miliard"
	| n `mod` 1000 <= 4 = "miliardy"
	| otherwise = "miliardów"

depolonize :: Char -> Char
depolonize c = case c of
	'ą' -> 'a'
	'ę' -> 'e'
	'ś' -> 's'
	'ć' -> 'c'
	'ż' -> 'z'
	'ź' -> 'z'
	'ń' -> 'n'
	_ -> c

shorten :: String -> String
shorten s = case s of
	't':'y':'s':_ -> "tys."
	'm':'i':'l':'i':'o':_ -> "mln."
	'm':'i':'l':'i':'a':_ -> "mld."
	'b':'i':'l':_ -> "bln."
	_ -> s

numToWord'' :: Int -> String
numToWord'' = unwords . map shorten . words . map depolonize . numToWord'

main = liftM (unlines . map (unwords . map shorten . words . map depolonize . numToWord' . read) . lines) (getLine >> getContents) >>= putStr
