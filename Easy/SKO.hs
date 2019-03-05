import Control.Monad

mean :: Int -> [Double] -> Double -> Double
mean n xs k = (sum (map (**k) xs) / fromIntegral n) ** (1/k)

find :: Int -> [Double] -> Int
find n grades = case filter (\k -> mean n grades (fromIntegral k) >= 4.0) [0..25] of
	k : _ -> k
	_ -> -1

main = do
	num <- liftM read getLine :: IO Int
	replicateM_ num $ do
		n <- liftM read getLine :: IO Int
		grades <- liftM (map read . words) getLine :: IO [Double]
		putStrLn . show $ find n grades
		
