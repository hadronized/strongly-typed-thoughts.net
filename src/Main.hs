import Happstack.Server ( Conf(port), nullConf, simpleHTTP )
import System.IO ( hPutStrLn, stderr )

import Routes

main :: IO ()
main = do
  portStr <- fmap reads getLine
  let [(p,φ)] = portStr
  if null portStr || not (null φ) then
    hPutStrLn stderr "unable to parse port"
    else do
      putStrLn $ "listening on " ++ show p
      simpleHTTP (nullConf { port = p }) routes
