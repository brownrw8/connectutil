module Common.Conn.Connection (query) where

import Database.HDBC 
import Database.HDBC.SqlValue 
import Database.HDBC.MySQL


defaultConnInfo = defaultMySQLConnectInfo { 
	mysqlHost 		= "127.0.0.1",
	mysqlUser 		= "root",
	mysqlPassword 	= "password",
	mysqlDatabase 	= "app",
	mysqlPort 		= 3306
}


query q =  do
	conn 	<- connect
	result 	<- sendRequest conn q
	disconnect conn
	return $ rawResult result

	
sendRequest conn q = catchSql (quickQuery' conn q []) handleError
	where handleError (SqlError s ne em) = do return [[toSql em, toSql $ show ne]]


connect = do
	conn <- connectMySQL defaultConnInfo
	return conn
	

rawResult rows = [rawRow x | x <- rows ]


rawRow row = [liftMySQLType x | x <- row ]


liftMySQLType x = case x of
	SqlString x -> show x
	SqlByteString x -> show x
	SqlWord32 x -> show x
	SqlWord64 x -> show x
	SqlInt32 x -> show x
	SqlInt64 x -> show x
	SqlInteger x -> show x
	SqlChar x -> show x
	SqlBool x -> show x
	SqlDouble x -> show x
	SqlRational x -> show x
	SqlLocalDate x -> show x
	SqlLocalTimeOfDay x -> show x
	SqlZonedLocalTimeOfDay x y -> ""
	SqlLocalTime x -> show x
	SqlZonedTime x -> show x
	SqlUTCTime x -> show x
	SqlDiffTime x -> show x
	SqlPOSIXTime x -> show x
	SqlEpochTime x -> show x
	SqlTimeDiff x -> show x
	SqlNull -> show "NULL"

	