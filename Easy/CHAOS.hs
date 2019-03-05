import Control.Monad

type Hours = Int
type Minutes = Int

data Time = Time Hours Minutes deriving Eq

instance Read Time where
	readsPrec _ (h1:h2:_:m1:m2:_) =
		let	h = read [h1, h2]
			m = read [m1, m2]
		in if 0 <= h && h <= 23 && 0 <= m && m <= 59
		then [(Time h m, "")]
		else []
	readsPrec _ _ = []

padded :: Int -> String
padded n = (if n < 10 then "0" else "") ++ show n

instance Show Time where
	show (Time h m) = padded h ++ ":" ++ padded m

next :: Time -> Time
next (Time h m)
	| m + 1 == 60 = Time ((h + 1) `mod` 24) 0
	| otherwise = Time h (m + 1)

isPalindrome :: Time -> Bool
isPalindrome (Time h m)
	| h == 0 = show m == reverse (show m)
	| otherwise = show h ++ padded m == reverse (show h ++ padded m)

nextPalindrome :: Time -> Time
nextPalindrome t = head $ filter isPalindrome (tail $ iterate next t)

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		t <- liftM read getLine :: IO Time
		putStrLn . show $ nextPalindrome t
