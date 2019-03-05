module Main where

import Control.Monad
import Data.Char

outer :: String -> String
outer [] = []
outer [x] = [x]
outer ('<':cs) = '<' : inner cs
outer (c:cs) = c : outer cs

inner :: String -> String
inner [] = []
inner [x] = [toUpper x]
inner ('>':cs) = '>' : outer cs
inner (c:cs) = toUpper c : inner cs

main = fmap outer getContents >>= putStr
