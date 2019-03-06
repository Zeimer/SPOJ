module Main where

import Control.Monad

transform :: String -> String
transform [] = []
transform ('?':'?':'=':cs) = '#' : transform cs
transform ('?':'?':'/':cs) = '\\' : transform cs
transform ('?':'?':'\'':cs) = '^' : transform cs
transform ('?':'?':'(':cs) = '[' : transform cs
transform ('?':'?':')':cs) = ']' : transform cs
transform ('?':'?':'!':cs) = '|' : transform cs
transform ('?':'?':'<':cs) = '{' : transform cs
transform ('?':'?':'>':cs) = '}' : transform cs
transform ('?':'?':'-':cs) = '~' : transform cs
transform (c:cs) = c : transform cs

main = fmap transform getContents >>= putStr
