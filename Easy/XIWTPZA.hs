import Control.Monad

data Rect = Rect Int Int

fits :: Rect -> Rect -> Bool
fits (Rect a b) (Rect a' b') = (a' < a && b' < b) || (a' < b && b' < a)

main = do
	l <- liftM (map (map read . words) . lines) (getLine >> getContents) :: IO [[Int]]
	forM_ l $ \[a, b, a', b'] -> do
		putStrLn $ if fits (Rect a b) (Rect a' b') then "TAK" else "NIE"
