module Main where

import Control.Applicative

main = (+) <$> fmap read getLine <*> fmap read getLine >>= putStrLn . show
