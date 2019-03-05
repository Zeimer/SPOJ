import Control.Monad

main = liftM (unlines . map (show . (\[v1, v2] -> 2 * v1 * v2 `div` (v1 + v2)) . map read . words) . lines) (getLine >> getContents) >>= putStr
