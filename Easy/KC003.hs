import Control.Monad

data Triangle = Bad | Good Double Double Double deriving Show

instance Read Triangle where	
	readsPrec _ str =
		let [a, b, c] = map read . take 3 . words $ str
		in if a >= 0 && b >= 0 && c >= 0
		then [(Good a b c, "")]
		else [(Bad, "")]

isValid :: Triangle -> Bool
isValid Bad = False
isValid (Good a b c) = a + b > c && a + c > b && b + c > a

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

main = liftM (map (boolToInt . isValid . read) . lines) getContents >>= mapM_ (putStrLn . show)
