import Control.Monad

count :: (Eq a) => a -> [a] -> Int
count n = length . filter (== n)

main = do
	str <- getContents
	forM_ (lines str) $ \line -> do
		let n : _ : seq = map read . words $ line :: [Integer]
		putStrLn . show $ count n seq
