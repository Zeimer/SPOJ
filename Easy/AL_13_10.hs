import Control.Monad

sparseSublist :: (Eq a) => [a] -> [a] -> Bool
sparseSublist _ [] = True
sparseSublist [] _ = False
sparseSublist (c:cs) (c':cs')
	| c == c' = sparseSublist cs cs'
	| otherwise = sparseSublist cs (c':cs')

boolToAns :: Bool -> String
boolToAns b = if b then "Tak" else "Nie"

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		[s, s'] <- liftM words getLine :: IO [String]
		putStrLn . boolToAns $ sparseSublist s s'
