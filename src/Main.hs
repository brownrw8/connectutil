import qualified Common.Conn.Connection as Conn
import qualified Common.Util.Utility as Util
import qualified System.Console.Haskeline as Hask
import qualified System.Environment as Env
import qualified System.IO as IO



main = do
	args <- Env.getArgs
	if ((==1) . length) args
		then do
			(action . (!!0)) args
			loop []
		else do	
			loop []
		

action query = do
	result <- (Conn.query . Util.escapeQuery) query
	defaultDisplay $ stripQuotes result


loop h = do
	putStr "\n>>> "
	IO.hFlush IO.stdout
	query <- getLine
	case query of
		"q"			-> return ()
		"Q"			-> return ()
		"quit"  	-> return ()
		"QUIT"		-> return ()
		":1"		-> do 
			let hist = reverse $ h !!0
			action hist
			loop (hist:h)
		":2"		-> do
			let hist = reverse $ h !!1
			action hist
			loop (hist:h)
		":3"		-> do 
			let hist = reverse $ h !!2
			action hist
			loop (hist:h)
		query		-> do
			action query
			loop (query:h)
			
			
stripQuotes result = map apply result
	where apply r = [filterChars x | x <- r]
		where filterChars r = [x | x <- r, x /= '"']

		
defaultDisplay result = mapM_ (putStrLn . show) result