import Control.Monad

main = liftM (show . length . lines) getContents >>= putStrLn
