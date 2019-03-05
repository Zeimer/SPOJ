import Control.Monad

divides :: Int -> Int -> Int -> Int -> Bool
divides n a b c = n `mod` a == 0 || n `mod` b == 0 || n `mod` c == 0

next :: Int -> Int -> Int
next start k = head $ filter (\n -> n `mod` k == 0) [start..]

prev :: Int -> Int -> Int
prev start k = head $ filter (\n -> n `mod` k == 0) [start, start-1..]

howMany :: Int -> Int -> Int -> Int
howMany sp sk a =
	let	start = next sp a
		end = prev sk a
	in 1 + (end - start) `div` a

howManyAll :: Int -> Int -> Int -> Int -> Int -> Int
howManyAll sp sk a b c = f a + f b + f c - f (a * b) - f (a * c) - f (b * c) + f (a * b * c)
	where f = howMany sp sk

{-main = do
	[a, b, c] <- liftM (map read . words) getLine :: IO [Int]
	s <- getContents
	forM_ (lines s) $ \line -> do
		let [sp, sk] = map read (words line)
		putStrLn . show $ howManyAll sp sk a b c-}

main = do
	lines <- liftM lines getContents
	let [a, b, c] = map read . words $ head lines
	forM_ (tail lines) $ \line -> do
		let [sp, sk] = map read (words line)
		putStrLn . show $ howManyAll sp sk a b c
