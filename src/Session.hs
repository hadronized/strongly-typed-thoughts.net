module Session (
  ) where

import Control.Lens
import Data.ByteString ( ByteString )
import Data.String ( fromString )
import Crypto.Hash.SHA3 ( hash )

data Session = Session {
    _sessionLogin  :: String
  , _sessionPasswd :: ByteString -- SHA3 hashed password 
  } deriving (Eq,Show)

makeLenses ''Session

createSession :: String -> String -> Session
createSession name passwd = Session name (hash 512 $ fromString passwd)
