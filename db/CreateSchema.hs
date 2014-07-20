module CreateSchema (
    createDB
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3

createDB :: String -> IO ()
createDB dbName = do
    conn <- connectSqlite3 dbName
    run conn createCredentialsSQL []
    commit conn
    disconnect conn

createCredentialsSQL :: String
createCredentialsSQL = unlines
    [
      "create table Credentials ("
    , "    credID   Integer primary key"
    , "  , credName VarChar(64) not null unique"
    , "  , credPwd  Bit(512) not null"
    , ");"
    ]
