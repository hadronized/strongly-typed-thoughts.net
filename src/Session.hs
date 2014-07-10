module Session (
    Cred(Cred)
  , credLogin
  , credPasswd
  , Session(Session)
  , sessionCred
  , session
  ) where

import Control.Lens
import Crypto.Hash.SHA3 ( hash )
import Data.ByteString ( ByteString )
import Data.SafeCopy ( base, deriveSafeCopy )
import Data.String ( fromString )
import Happstack.Server.ClientSession ( ClientSession(..) )

data Cred = Cred {
    _credLogin  :: String
  , _credPasswd :: ByteString -- SHA3 hashed password 
  } deriving (Eq,Show)

newtype Session = Session { _sessionCred :: Maybe Cred } deriving (Eq,Show)

instance ClientSession Session where
  emptySession = Session Nothing
 
makeLenses ''Cred
makeLenses ''Session
deriveSafeCopy 0 'base ''Cred
deriveSafeCopy 0 'base ''Session

session :: String -> String -> Session
session name passwd = Session . Just $ Cred name (hash 512 $ fromString passwd)
