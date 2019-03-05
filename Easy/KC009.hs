module Main where

import Control.Monad

main = liftM (unlines . map reverse . lines) getContents >>= putStr
