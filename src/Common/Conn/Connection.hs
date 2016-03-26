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
	SqlString x -> ("String",show x)
	SqlByteString x -> ("ByteString",show x)
	SqlWord32 x -> ("Word32",show x)
	SqlWord64 x -> ("Word64",show x)
	SqlInt32 x -> ("Int32",show x)
	SqlInt64 x -> ("Int64",show x)
	SqlInteger x -> ("Integer",show x)
	SqlChar x -> ("Char",show x)
	SqlBool x -> ("Bool",show x)
	SqlDouble x -> ("Double",show x)
	SqlRational x -> ("Rational",show x)
	SqlLocalDate x -> ("LocalDate",show x)
	SqlLocalTimeOfDay x -> ("LocalTimeOfDay",show x)
	SqlZonedLocalTimeOfDay x y -> ("ZonedLocalTimeOfDay"++(show y),show x)
	SqlLocalTime x -> ("LocalTime",show x)
	SqlZonedTime x -> ("ZonedTime",show x)
	SqlUTCTime x -> ("UTCTime",show x)
	SqlDiffTime x -> ("DiffTime",show x)
	SqlPOSIXTime x -> ("PosixTime",show x)
	SqlEpochTime x -> ("EpochTime",show x)
	SqlTimeDiff x -> ("TimeDiff",show x)
	SqlNull -> ("Null","NULL")

	