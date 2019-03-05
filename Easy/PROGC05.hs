import Control.Monad

main = do
	l <- liftM lines getContents :: IO [String]
	forM_ l $ \(h : _ : t) -> do
		putStrLn $ filter (/= h) t
