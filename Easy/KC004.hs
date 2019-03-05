import Control.Monad
import qualified Data.ByteString.Char8 as BS

count :: (Eq a) => a -> [a] -> Int
count n = length . filter (== n)

main = do
	str <- getContents
	forM_ (lines str) $ \line -> do
		let n : _ : seq = map read . words $ line :: [Integer]
		BS.putStrLn . BS.pack . show $ count n seq
