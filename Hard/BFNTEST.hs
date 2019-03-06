import Control.Monad

data State = Keep | Line | Block

transition :: String -> State -> (String, State)
transition str st = case (st, str) of
	(Keep, "//":t)	-> (t, Line)
	(Keep, "/*":t)	-> (t, Block)
	(Keep, _)	-> (str, Keep)
	(Line, "\n":t)	-> (t, Keep)
	(Line, _:t)	-> (t, Line)
	(Block, "*/":t)	-> (t, Keep)
	(Block, _:t)	-> (t, Block)


