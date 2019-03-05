import Control.Monad
import Data.List
import qualified Data.Map as M

{-import System.Random
import System.IO

genData :: Int -> Int -> IO [String]
genData n k = replicateM n (replicateM k (randomRIO ('A', 'D')))

writeData :: FilePath -> Int -> Int -> IO ()
writeData path n k = do
	s <- genData n k
	writeFile path (unlines s)

findBiggestMultiset :: (Ord a) => [[a]] -> Int
findBiggestMultiset = maximum . map length . group . sort . map sort

findBiggestMultiset' :: (Ord a) => [[a]] -> Int
findBiggestMultiset' l = n where
	l' = map sort l
	m = Prelude.foldr (\str acc -> if M.member str acc then M.update (\n -> Just (n + 1)) str acc else M.insert str 1 acc) M.empty l'
	n = maximum . map snd $ M.toList m-}

findBiggestMultiset2 :: (Ord a) => [[a]] -> Int
findBiggestMultiset2 = length . maximumBy (\x y -> compare (length x) (length y)) . group . sort . map sort

main = do
	n <- liftM read getLine :: IO Int
	replicateM_ n $ do
		k <- liftM read getLine :: IO Int
		l <- replicateM k getLine :: IO [String]
		putStrLn . show $ findBiggestMultiset2 l
