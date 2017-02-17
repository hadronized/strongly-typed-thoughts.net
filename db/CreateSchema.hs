{-# LANGUAGE OverloadedStrings #-}

module CreateSchema (
    createDB
  ) where

import Data.Text as T ( unlines )
import Database.SQLite.Simple ( Query(..), execute_, withConnection, withTransaction )

createDB :: String -> IO ()
createDB dbName =
  withConnection dbName $ \conn -> withTransaction conn (execute_ conn createCredentialsQuery)

createCredentialsQuery :: Query
createCredentialsQuery = Query $ T.unlines
    [
      "create table Credentials ("
    , "  cred_id Integer primary key,"
    , "  cred_name VarChar(64) not null unique,"
    , "  cred_pwd Bit(512) not null"
    , ");"
    ]
