import Control.Monad
import Data.List
import Data.Char

main = do
	strs <- liftM (group . sort) getContents :: IO [String]
	forM_ strs $ \str -> do
		putStrLn $ show (ord $ head str) ++ " " ++ show (length str)
