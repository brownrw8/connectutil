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
		else do	
			loop
		

action query = do
	result <- (Conn.query . Util.escapeQuery) query
	defaultDisplay result


loop = do
	putStr "\n>>> "
	IO.hFlush IO.stdout
	query <- getLine
	case query of
		"q"			-> return ()
		"Q"			-> return ()
		"quit"  	-> return ()
		"QUIT"		-> return ()
		query		-> do
			action query
			loop
			

		
defaultDisplay result = mapM_ (putStrLn . show . display) result
display [] = []
display (x:[]) = (\(a,b) -> (a++"::"++(stripQuotes b))) x : []
display (x:xs) = (\(a,b) -> (a++"::"++(stripQuotes b))) x : display xs 

stripQuotes s = [x | x <- s, x /= '"']