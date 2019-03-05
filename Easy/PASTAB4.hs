import Control.Monad
import Data.List

type Histogram = [(Int, Int)]

makeHistogram :: [Int] -> Histogram
makeHistogram l = map (\k -> (k, length (filter (== k) l))) [-10..10]

bigLCM :: [Int] -> Int
bigLCM = foldr (\a b -> if a == 0 then b else lcm a b) 1

adjustHistogram :: Histogram -> Histogram
adjustHistogram h = map (\(k, len) -> (k, if len == 0 then 0 else len * n `div` m)) h where
	n = bigLCM (map snd h)
	m = maximum (map snd h)

pad :: Int -> String
pad k
	| k < - 10 = error "WUT"
	| k == -10 = ' ' : show k
	| -10 < k && k < 0 = "  " ++ show k
	| 0 <= k && k < 10 = "   " ++ show k
	| k == 10 = "  " ++ show k
	| k > 10 = error "WUT"

main = do
	histogram <- liftM (adjustHistogram . makeHistogram . map read . lines) getContents :: IO Histogram
	
	let maxWidth = maximum $ map snd histogram

	forM_ histogram $ \(k, len) -> do
		--putStrLn $ pad k ++ ":|" ++ replicate len '*' ++ replicate (maxWidth - len) ' ' ++ "|"
		let width = round (fromIntegral (30 * len) / fromIntegral maxWidth)
		putStrLn $ pad k ++ ":|" ++ replicate width '*' ++ replicate (30 - width) ' ' ++ "|"
