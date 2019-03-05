import Control.Monad

dist :: [Double] -> Double
dist [x1, y1, x2, y2] = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

main = do
	coords1 <- liftM (map read . words) getLine :: IO [Double]
	coords2 <- liftM (map read . words) getLine :: IO [Double]
	let	d1 = dist coords1
		d2 = dist coords2
		p1 = d1^2 / 2
		p2 = d2^2 / 2
		p = p1 + p2
	putStrLn . show $ p2
