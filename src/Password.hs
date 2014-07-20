module Password (
    hashPwd
  ) where

import Crypto.Hash.SHA3 ( hash )
import Data.ByteString ( ByteString )
import Data.String ( fromString )

hashPwd :: String -> ByteString
hashPwd = hash 512 . fromString
