import Database.HDBC
import Database.HDBC.Sqlite3
import Password ( hashPwd )
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )

main :: IO ()
main = do
    args <- getArgs
    if length args == 2
    then do
      let [login,pwd] = args
      putStrLn "connecting to database..."
      conn <- connectSqlite3 "db/local.db"
      run conn addUserSQL [SqlString login,SqlByteString $ hashPwd pwd]
      commit conn
      disconnect conn
    else
      hPutStrLn stderr "syntax error"

addUserSQL :: String
addUserSQL = "insert into Credentials (credName,CredPwd) VALUES (?,?)"
