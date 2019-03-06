import Control.Monad

type Reg = Integer -> Integer

emptyReg :: Reg
emptyReg _ = 0

go :: [String] -> Reg -> [Integer]
go [] _ = []
go (s:ss) r =
	let	w = words s
		op = head w
		[arg1, arg2] = map read $ tail w :: [Integer]
	in case op of
		"z" -> go ss (\i -> if i == arg1 then arg2 else r i)
		"+" -> (r arg1 + r arg2) : go ss r
		"-" -> (r arg1 - r arg2) : go ss r
		"*" -> (r arg1 * r arg2) : go ss r
		"/" -> (r arg1 `div` r arg2) : go ss r
		"%" -> (r arg1 `mod` r arg2) : go ss r

main = do
	c <- getContents
	mapM_ (putStrLn . show) (go (lines c) emptyReg)
