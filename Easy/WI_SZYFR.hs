import Control.Monad
import Data.Char

data Command = Cipher | Decipher

instance Show Command where
	show Cipher = "SZYFRUJ"
	show Decipher = "DESZYFRUJ"

instance Read Command where
	readsPrec _ str = case str of
		"SZYFRUJ" -> [(Cipher, "")]
		"DESZYFRUJ" -> [(Decipher, "")]

type Key = [Int]

makeKey :: String -> Key
makeKey = cycle . map (read . return)

cipherChar :: Int -> Char -> Char
cipherChar k c = chr $ 65 + (ord c - 65 + k) `mod` 26

cipherString :: Key -> String -> String
cipherString = zipWith cipherChar

decipherChar :: Int -> Char -> Char
decipherChar k c = chr $ 65 + (ord c - 65 - k) `mod` 26

decipherString :: Key -> String -> String
decipherString = zipWith decipherChar

execCmd :: Command -> Key -> String -> String
execCmd Cipher = cipherString
execCmd Decipher = decipherString

main = liftM3 execCmd (fmap read getLine) (fmap makeKey getLine) getLine
{-do
	cmd <- liftM read getLine :: IO Command
	key <- liftM (makeKey . read) getLine :: IO Key
	str <- getLine :: IO String
	case cmd of-}
