import Control.Monad

data Point a = P a a

data Line a = L a a

lineFromPoints :: Point Double -> Point Double -> Line Double
lineFromPoints (P x1 y1) (P x2 y2) = L a b where
	a = if y1 == y2 then 0.0 else (y1 - y2) / (x1 - x2)
	b = y1 - a * x1

atSkewLine :: Point Double -> Line Double -> Bool
atSkewLine (P x y) (L a b) = a * x + b == y

atVerticalLine :: Point Double -> Point Double -> Point Double -> Bool
atVerticalLine (P x1 y1) (P x2 y2) (P x3 y3) = (x1 == x2 && x2 == x3) || (y1 == y2 && y2 == y3)

atLine :: Point Double -> Point Double -> Point Double -> Bool
atLine p1 p2 p3 = atSkewLine p1 (lineFromPoints p2 p3) || atVerticalLine p1 p2 p3

solve :: String -> IO ()
solve str = do
	forM_ (tail $ lines str) $ \line -> do
		let [x1, y1, x2, y2, x3, y3] = map read (words line)
		putStrLn $ if atLine (P x1 y1) (P x2 y2) (P x3 y3) then "TAK" else "NIE"

main = getContents >>= solve
