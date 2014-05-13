import Happstack.Server ( nullConf, simpleHTTP )

import Routes

main :: IO ()
main = simpleHTTP nullConf routes