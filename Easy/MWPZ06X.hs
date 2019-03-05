module Main where

import Control.Monad

{-main = getLine >> liftM (unlines . map show . map (^2) . map read . lines) getContents >>= putStr-}

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Integer
		putStrLn . show $ k^2
