import Control.Monad
import Text.Printf

data Solution = R Double | BR | NWR

instance Show Solution where
	show NWR = "NWR"
	show BR = "BR"
	show (R x) = printf "%.2f" x

round2 :: Double -> Double
round2 x = (fromInteger $ round $ x * (10^2)) / (10.0^^2)

solve :: Double -> Double -> Double -> Solution
solve 0.0 b c
	| b == c = NWR
	| otherwise = BR
solve a b c = R . round2 $ (c - b)/a

main = liftM (show . (\[a, b, c] -> solve a b c) . map read . words) getLine >>= putStrLn
