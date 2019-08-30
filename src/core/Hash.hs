module Hash (mkHash) where

import Control.Lens.Iso.Extended    
import Control.Lens
import Crypto.Hash
import Data.ByteString

mkHash :: Show a => a -> ByteString
mkHash x = show (hash (show x^.stext.textbs) :: SHA256)^.stext.textbs