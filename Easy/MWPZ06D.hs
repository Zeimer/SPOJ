import Control.Monad

answer :: Integer -> Integer -> String
answer l c
	| l == 1 = "TAK"
	| otherwise = if c `mod` (l - 1) == 0 then "NIE" else "TAK"

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[l, c] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn (answer l c)
