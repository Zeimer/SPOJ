import Control.Monad
import Data.List

showLine :: [Int] -> String
showLine = join . intersperse " " . map show

main = do
	d <- liftM read getLine :: IO Int
	replicateM_ d $ do
		getLine
		l <- liftM (map read . words) getLine :: IO [Int]
		let	l' = sortBy (\x y -> compare y x) l
			(hs, ts) = span (== head l') l'
		putStrLn . showLine $ hs ++ reverse ts
