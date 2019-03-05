module Main where

import Control.Monad

main = liftM (unlines . map (show . (\n -> n - 1) . sum . map read . words) . lines) (getLine >> getContents) >>= putStr
