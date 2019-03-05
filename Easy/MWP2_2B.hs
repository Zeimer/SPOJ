import Control.Monad
import Data.Bits

modPow :: Integer -> Integer -> Integer -> Integer -> Integer
modPow acc u s d = case s of
	0 -> acc
	_ -> modPow (acc * u `mod` d) u (s - 1) d

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  		   where t = if testBit e 0 then b `mod` m else 1
{-main = do
	[u, s, d] <- liftM (map read . words) getLine :: IO [Integer]
	when (any (/= 0) [u, s, d]) $ do
		putStrLn . show $ modExp u s d
		main
-}

main = do
	l <- liftM (map (map read . words) . lines) getContents
	let l' = takeWhile (any (/= 0)) l
	forM_ l' $ \[u, s, d] -> do
		putStrLn . show $ modExp u s d
