import Control.Monad
import Data.List

quickSelect :: (Eq a) => (a -> a -> Bool) -> Int -> [a] -> Maybe a
quickSelect cmp n l = aux n (nub l) where
	aux _ [] = Nothing
	aux n (x:xs)
		| n < 0 = Nothing
		| otherwise =
			let (l1, l2) = partition (`cmp` x) xs
			in if n < length l1
			then aux n l1
			else if n > length l1
				then aux (n - length l1 - 1) l2
				else Just x

main = do
	lines <- liftM lines getContents :: IO [String]
	forM_ lines $ \line -> do
		let	i:l = map read (words line) :: [Int]
		putStrLn $ case quickSelect (>=) (i - 1) l of
			Just n -> show n
			_ -> "-"
