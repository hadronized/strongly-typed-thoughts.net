import Happstack.Server ( Conf(port), nullConf, simpleHTTP )
import System.IO ( hPutStrLn, stderr )
import System.Directory

import CreateSchema
import Routes

main :: IO ()
main = do
  portStr <- fmap reads getLine
  let [(p,φ)] = portStr
  if null portStr || not (null φ) then
    hPutStrLn stderr "unable to parse port"
    else do
      putStrLn $ "listening on " ++ show p
      initiateDB
      simpleHTTP (nullConf { port = p }) routes

-- If the database is not present yet, that function creates it. Otherwise,
-- it does nothing.
initiateDB :: IO ()
initiateDB = do
    already <- doesFileExist "db/local.db"
    if not already then do
      putStrLn "initiating database..."
      createDB "db/local.db"
      else
        return ()

