import Control.Monad

data Weekday = Pn | Wt | Sr | Cz | Pt | So | Nd deriving (Read, Show, Enum)

next :: Weekday -> Weekday
next Nd = Pn
next d = succ d



main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[d, k] <- liftM words getLine :: IO [String]
		putStrLn . show $ iterate next (read d) !! (read k `mod` 7)
