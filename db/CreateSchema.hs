module CreateSchema where

import Database.HDBC
import Database.HDBC.Sqlite3

-- This module is used at Setup to initiate sql database. It can also be used
-- to generate the database manually.
createDB :: String -> IO ()
createDB dbName = do
    conn <- connectSqlite3 dbName
    run conn creatCredentialsSQL []
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
