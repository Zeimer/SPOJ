import Control.Monad
import Test.QuickCheck

toRoman :: Int -> String
toRoman n
	| n <= 0 = ""
	| n == 1 = "I"
	| n == 2 = "II"
	| n == 3 = "III"
	| n == 4 = "IV"
	| n == 5 = "V"
	| n == 6 = "VI"
	| n == 7 = "VII"
	| n == 8 = "VIII"
	| n == 9 = "IX"
	| n == 10 = "X"
	| 11 <= n && n <= 39 = 'X' : toRoman (n - 10)
	| 40 <= n && n <= 49 = "XL" ++ toRoman (n - 40)
	| 50 <= n && n <= 89 = 'L' : toRoman (n - 50)
	| 90 <= n && n <= 99 = "XC" ++ toRoman (n - 90)
	| 100 <= n && n <= 399 = 'C' : toRoman (n - 100)
	| 400 <= n && n <= 499 = "CD" ++ toRoman (n - 400)
	| 500 <= n && n <= 899 = 'D' : toRoman (n - 500)
	| 900 <= n && n <= 999 = "CM" ++ toRoman (n - 900)
	| n == 1000 = "M"
	| otherwise = "WUT"

fromRoman :: String -> Int
fromRoman [] = 0
fromRoman ('I':'V':cs) = 4 + fromRoman cs
fromRoman ('I':'X':cs) = 9 + fromRoman cs
fromRoman ('X':'L':cs) = 40 + fromRoman cs
fromRoman ('X':'C':cs) = 90 + fromRoman cs
fromRoman ('C':'D':cs) = 400 + fromRoman cs
fromRoman ('C':'M':cs) = 900 + fromRoman cs
fromRoman ('I':cs) = 1 + fromRoman cs
fromRoman ('V':cs) = 5 + fromRoman cs
fromRoman ('X':cs) = 10 + fromRoman cs
fromRoman ('L':cs) = 50 + fromRoman cs
fromRoman ('C':cs) = 100 + fromRoman cs
fromRoman ('D':cs) = 500 + fromRoman cs
fromRoman ('M':cs) = 1000 + fromRoman cs

romanSum :: String -> String -> String
romanSum s s' = toRoman (fromRoman s + fromRoman s')

prop :: Int -> Int -> Bool
prop a b
	| a < 0 || a > 1000 = True
	| b < 0 || b > 1000 = True
	| otherwise = toRoman (a + b) == romanSum (toRoman a) (toRoman b)

main = getContents >>= putStr . unlines . map ((\[s, s'] -> romanSum s s') . words) . lines
