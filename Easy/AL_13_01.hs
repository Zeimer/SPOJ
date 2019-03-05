import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as BS

solve :: Int -> [Int]
solve n = aux [n] [n-1,n-2..1] where

	aux acc [] = acc
	aux acc (x:xs) =
		let	acc' = last acc : init acc
		in aux (x:acc') xs

test :: [a] -> [a]
test [] = []
test [x] = [x]
test (x:y:zs) = x : test (zs ++ [y])

showList' :: (Show a) => [a] -> BS.ByteString
showList' l = BS.intercalate (BS.pack " ") $ map (BS.pack . show) l --intercalate " " (map show l)

main = do
	l <- liftM (tail . lines) getContents
	forM_ l $ \line -> do
		let size = read line
		BS.putStrLn . showList' $ solve size
