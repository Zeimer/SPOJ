import Control.Monad

runStack :: (Read a, Show a) => [a] -> [String] -> [String]
runStack _ [] = []
runStack stack cmds = case cmds of
	"-":cmds' -> if null stack then ":(" : runStack stack cmds' else show (head stack) : runStack (tail stack) cmds'
	"+":num:cmds' -> if length stack >= 10 then ":(" : runStack stack cmds' else ":)" : runStack (read num : stack) cmds'
	_:cmds' -> ":(" : runStack stack cmds'

runStack' :: [Int] -> [String] -> [String]
runStack' = runStack

main = getContents >>= putStr . unlines . runStack' [] . lines
