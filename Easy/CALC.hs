module Main where

import Control.Monad

main = do
	s <- getContents
	forM_ (lines s) $ \line -> do
		let [op, arg1, arg2] = words line
		putStrLn . show $ case op of
			"+" -> read arg1 + read arg2 :: Int
			"-" -> read arg1 - read arg2 :: Int
			"*" -> read arg1 * read arg2 :: Int
			"/" -> read arg1 `div` read arg2 :: Int
			"%" -> read arg1 `mod` read arg2 :: Int
			_ -> error "ERROR"
