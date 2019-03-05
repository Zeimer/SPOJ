import Control.Monad

data Circle a = C a a a deriving Show

instance (Read a) => Read (Circle a) where
	readsPrec _ str = [(C (read x) (read y) (read r), "")] where
		[x, y, r] = take 3 $ words str

data Point a = P a a deriving Show

instance (Read a) => Read (Point a) where
	readsPrec _ str = [(P (read x) (read y), "")] where
		[x, y] = take 2 $ words str

center :: Circle a -> Point a
center (C x y _) = P x y

radius :: Circle a -> a
radius (C _ _ r) = r

data Loc = I | E | O deriving Show

fromOrdering :: Ordering -> Loc
fromOrdering o = case o of
	LT -> I
	EQ -> E
	GT -> O

dist :: Point Double -> Point Double -> Double
dist (P x1 y1) (P x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

locate :: Point Int -> Circle Int -> Loc
locate p c = fromOrdering (compare (dist p' (center c')) (radius c')) where
	p' = case p of
		P x y -> P (fromIntegral x) (fromIntegral y)
	c' = case c of
		C x y r -> C (fromIntegral x) (fromIntegral y) (fromIntegral r)

main = do
	c <- liftM read getLine :: IO (Circle Int)
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		p <- liftM read getLine :: IO (Point Int)
		putStrLn . show $ locate p c
