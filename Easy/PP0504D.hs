import Control.Monad
import Data.Word
import Data.Bits
import Unsafe.Coerce

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

bitsToList :: (Bits a) => a -> [Int]
bitsToList x = map (boolToInt . testBit x) [31,30..0]

intToHexDigit :: Int -> Char
intToHexDigit n
	| n < 10 = head $ show n
	| otherwise = case n of
		10 -> 'a'
		11 -> 'b'
		12 -> 'c'
		13 -> 'd'
		14 -> 'e'
		15 -> 'f'

bitsToHexDigit :: [Int] -> Char
bitsToHexDigit [a, b, c, d] = intToHexDigit $ 8 * a + 4 * b + 2 * c + d

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n l = take n l : groups n (drop n l)

formatHex :: String -> String
formatHex [] = []
formatHex (b:b':bs) = if b == '0' then b' : ' ' : formatHex bs else b : b' : ' ' : formatHex bs

bitsToHex :: (Bits a) => a -> String
bitsToHex x = formatHex . map bitsToHexDigit . groups 4 . bitsToList $ x

floatToHex :: Float -> String
floatToHex x = bitsToHex (unsafeCoerce x :: Word32)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		x <- liftM read getLine :: IO Float
		putStrLn . floatToHex $ x
