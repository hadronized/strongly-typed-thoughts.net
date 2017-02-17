{-# LANGUAGE OverloadedStrings #-}

import Data.String ( IsString(..) )
import Data.Text ( Text )
import Database.SQLite.Simple ( Query, execute, withConnection, withTransaction )
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
    withConnection "db/local.db" $ \conn -> withTransaction conn $ do
      execute conn addUserQuery (fromString login :: Text, hashPwd pwd)
  else
    hPutStrLn stderr "syntax error"

addUserQuery :: Query 
addUserQuery = "insert into Credentials (cred_name, cred_pwd) values (?,?)"
