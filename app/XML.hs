module XML (
  XML
  ) where

import qualified Data.ByteString.Lazy as BS (empty)
import Data.Default (Default(..))
import Data.XML.Types (Element(..))
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..), MimeRender(..))
import Text.RSS.Syntax (RSS)
import Text.RSS.Export (xmlRSS)
import Text.XML (Document(..), Prologue(..), fromXMLElement, renderLBS)

data XML

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance (ToXML a) => MimeRender XML a where
  mimeRender _ =  either (const BS.empty) render . fromXMLElement . toXML
    where
      render = renderLBS def . toDocument
      prologue = Prologue [] Nothing []
      epilogue = []
      toDocument el = Document prologue el epilogue

class ToXML a where
  toXML :: a -> Element

instance ToXML RSS where
  toXML = xmlRSS
