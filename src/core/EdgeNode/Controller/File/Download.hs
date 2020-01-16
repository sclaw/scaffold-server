module EdgeNode.Controller.File.Download (controller) where

import EdgeNode.Transport.Id

import KatipController
import Network.Wai
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types


controller :: Id -> KatipController Application
controller _ = do
  _ <- ask
  return $ \_ resp -> resp $ responseLBS status200 [] BL.empty