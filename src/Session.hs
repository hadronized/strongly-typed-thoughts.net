module Session (
    Cred(Cred)
  , credLogin
  , credPasswd
  , Session(Session)
  , sessionCred
  , session
  ) where

import Control.Lens
import Data.ByteString ( ByteString )
import Data.SafeCopy ( base, deriveSafeCopy )
import Happstack.Server.ClientSession ( ClientSession(..) )

import Password

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
session name passwd = Session . Just $ Cred name (hashPwd passwd)
