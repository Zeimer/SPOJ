import Control.Monad

($>) = flip ($)

-- Slow.
solve :: (Num a, Eq a) => [a] -> Int
solve l = [1..length l - 1] $> map (\n -> splitAt n l) $> map (\(l1, l2) -> sum l1 == sum l2) $> zip [1..] $> filter (\(_, b) -> b == True) $> \l' -> case l' of
	[] -> 0
	h:_ -> fst h

prefix :: (Num a) => [a] -> [a]
prefix = scanl (+) 0

suffix :: (Num a) => [a] -> [a]
suffix = reverse . scanl (+) 0 . reverse

mid :: [a] -> [a]
mid = init . tail

-- Fast, but SPOJ marudzi anyway.
solve' :: (Num a, Eq a) => [a] -> Int
solve' l =
	let	p = prefix l
		s = suffix l
	in zipWith (==) (mid p) (mid s) $> zip [1..] $> filter ((== True) . snd) $> \l' -> case l' of
		[] -> 0
		h:_ -> fst h

{-main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		_:l <- liftM (map read . words) getLine :: IO [Int]
		putStrLn . show $ solve' l-}

main = do
	s <- getContents
	forM_ (tail $ lines s) $ \line -> do
		let _:l = map read (words line)
		putStrLn . show $ solve' l
