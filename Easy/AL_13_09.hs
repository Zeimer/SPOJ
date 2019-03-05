import Control.Monad
import Data.List

newton :: Int -> (Double -> Double) -> (Double -> Double) -> Double -> Double
newton n f f' x
	| n <= 0 = x
	| otherwise = let xn = newton (n - 1) f f' x in xn - f xn / f' xn

solutions :: Double -> Double -> Double -> [Double]
solutions a b m
	| abs m > 1 = []
	| otherwise = nub $ s1 ++ s2 ++ s3 ++ s4 where

			x = newton 100 (\x -> sin x - m) cos 0
			x' = pi - x
			
			grab = takeWhile (\x -> a*pi <= x && x <= b*pi)

			s1 = grab [x, x + 2*pi ..]
			s2 = grab [x, x - 2*pi ..]
			s3 = grab [x', x' + 2*pi ..]
			s4 = grab [x', x' - 2*pi ..]

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[a, b, m] <- liftM (map read . words) getLine :: IO [Double]
		putStrLn . show . length $ solutions a b m
