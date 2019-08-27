module Hash (mkHash) where

import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Data.ByteString (ByteString)
import Control.Lens.Iso.Extended    
import Control.Lens

mkHash :: Show a => a -> ByteString
mkHash x = convertToBase Base64 (show x^.stext.textbs)
