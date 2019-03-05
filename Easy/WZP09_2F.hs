import Control.Monad

{-main = getContents >>= putStr . unlines . map (\str -> if read str `mod` 15 == 0 then "TAK" else "NIE") . lines-}

main = do
	n <- liftM read getLine :: IO Integer
	when (n /= 0) $ do
		putStrLn $ if n `mod` 15 == 0 then "TAK" else "NIE"
		main
