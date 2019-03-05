import Control.Monad

myMod :: Integer -> Integer -> Integer
myMod a b = if a < 0 then 1 - a `mod` b else a `mod` b

{-main = liftM (map (map read . words) . lines) (getLine >> getContents) >>= mapM_ (putStrLn . show . (\[a, b] -> a `mod` b))-}

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b] <- liftM (map read . words) getLine :: IO [Integer]
		putStrLn . show $ a `mod` b
