module EdgeNode.Service.Data.Google (Items (..)) where

import qualified Data.Text as T
import Data.Aeson
import qualified Data.Vector as V

newtype Items = Items [T.Text]
  
instance FromJSON Items where
  parseJSON = withObject "Items.stage1" $ \o -> do 
    o' <- o .: "data"
    o'' <- o' .: "translations"
    let f v = 
         withObject 
         "Items.stage3" 
         (.: "translatedText") 
         (v V.! 0) 
    (Items . T.splitOn ",") `fmap` withArray "Items.stage2" f o''