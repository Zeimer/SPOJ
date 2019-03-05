import Control.Monad
import Data.List

notEnoughFives :: Integer -> Integer -> Integer
notEnoughFives goal n =
	let	num = genericLength (filter (== '5') (show n))
	in if goal <= num
	then n
	else
		let	(_, k) = head (filter (\x -> fst x /= '5') (zip (reverse (show n) ++ ['0','0'..]) [0..]))
		in notEnoughFives goal (n + 10 ^ k)

down :: Integer -> Integer -> Integer
down goal n = aux r l where

	l = zip (reverse (show n)) [0..]

	r = genericLength (filter (\x -> fst x == '5') l) - goal

	aux _ [] = 0
	aux 0 ((c, k):rest) = read [c] * 10 ^ k + aux 0 rest
	aux 1 (('5', k):rest) = 6 * 10 ^ k + aux 0 rest
	aux r ((c, k):rest)
		| c == '5' = aux (r - 1) rest
		| otherwise = (read [c]) * 10 ^ k + aux r rest

naive :: Integer -> Integer -> Integer
naive goal n = head $ filter (\num -> genericLength (filter (== '5') (show num)) == goal) [n+1..]

solve :: Integer -> Integer -> Integer
solve goal n = down goal $ notEnoughFives goal (n + 1)

main = do
	[n, k] <- liftM (map read . words) getLine :: IO [Integer]
	putStrLn . show $ solve k n
