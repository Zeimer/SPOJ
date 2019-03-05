import Control.Monad
import Data.List
import Data.Ord
import qualified Data.ByteString.Char8 as BS

allCycles :: Int -> [a] -> [[a]]
allCycles n = take n . map (take n) . tails . cycle

howMany :: (Eq a) => [a] -> [a] -> Int
howMany l1 l2 = sum $ zipWith (\x y -> if x == y then 1 else 0) l1 l2

longest :: Int -> [Int] -> [Int]
longest n l = snd . head . sortBy (flip $ comparing fst) $ map (\l -> (howMany [1..n] l, l)) (allCycles n l)

showL :: (Show a) => [a] -> BS.ByteString
showL = BS.intercalate (BS.pack " ") . map (BS.pack . show)

main = do
	numOfIters <- liftM read getLine :: IO Int
	replicateM_ numOfIters $ do
		n <- liftM read getLine :: IO Int
		l <- liftM (map read . words) getLine :: IO [Int]
		BS.putStrLn . showL $ longest n l
