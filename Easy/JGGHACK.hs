import Control.Monad
import Data.Char

c1 :: Char -> Int
c1 c = case c of
	'A' -> 0
	'B' -> 1
	'C' -> 2
	'D' -> 3
	'E' -> 4
	'F' -> 5
	'G' -> 6 
	'H' -> 7
	'I' -> 8
	'J' -> 9
	'K' -> 10
	'L' -> 11
	'M' -> 12
	'N' -> 13
	'O' -> 14
	'P' -> 15

c2 :: Char -> Int
c2 c = case c of
	'A' -> 0
	'B' -> 16
	'C' -> 32
	'D' -> 48
	'E' -> 64
	'F' -> 80
	'G' -> 96
	'H' -> 112
	'I' -> 128
	'J' -> 144
	'K' -> 160
	'L' -> 176
	'M' -> 192
	'N' -> 208
	'O' -> 224
	'P' -> 240

decode :: String -> String
decode "" = ""
decode [x] = error "ERROR"
decode (x:y:zs) = let n = c1 x + c2 y in (if n <= 128 then chr n else chr $ 255 - n) : decode zs

main = liftM (unlines . map decode . lines) getContents >>= putStr
