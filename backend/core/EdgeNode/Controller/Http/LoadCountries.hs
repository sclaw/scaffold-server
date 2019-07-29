{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Http.LoadCountries (controller) where

import EdgeNode.Service.Data (ServiceLanguage (..))
import EdgeNode.Service.Countries
import EdgeNode.Service.Data.Google 

import ReliefJsonData
import KatipController
import RetrofitProto
import qualified Data.Text as T
import Control.Monad.Reader.Class
import Network.HTTP.Client
import Data.String.Interpolate
import Data.Traversable
import Control.Lens
import Control.Monad.IO.Class
import Control.Lens.Iso.Extended
import Data.Aeson
import qualified Data.Vector as V
import Control.Monad (join)

newtype Country = Country T.Text

instance FromJSON Country where
    parseJSON = withObject "Country" $ \o -> Country `fmap` (o .: "name")
  
controller :: ServiceLanguage -> KatipController (Alternative T.Text CountriesResponse)
controller lang = 
  do
    keys <- (^.katipEnv.apiKeys) `fmap` ask
    mgr <- (^.katipEnv.httpReqManager) `fmap` ask 
    resp <- liftIO $ for (lookup "google" keys) $ \key -> do
      let countryQuery = [i|https://restcountries.eu/rest/v2/all|]
      countryReq <- parseRequest countryQuery
      countryResp <- httpLbs countryReq mgr
      responseClose countryResp
      for (eitherDecode (responseBody countryResp)) $ \(xs :: [Country]) ->
        case lang of
          En -> return $ Right $ map (^.coerced) xs
          Ru ->  do 
           let xs' = T.intercalate "," $ map (^.coerced) xs
           let xsQuery = [i|https://translation.googleapis.com/language/translate/v2?key=#{key}&q=#{xs'}&target=#{lang}&format=text&source=en|]
           xsReq <- parseRequest xsQuery
           xsResp <- httpLbs xsReq mgr
           responseClose xsResp
           let body = responseBody xsResp
           case eitherDecode body of 
             Right (Items xs) -> return $ Right $ xs^..traverse.to T.strip
             Left e -> return $ Left e 
    let mkOk (Right xs) = Fortune $ CountriesResponse $ Response (V.fromList (xs^..traverse.from lazytext))
        mkOk (Left e) = ReliefJsonData.Error $ e^.stext          
    return $ maybe (ReliefJsonData.Error "api key not found") mkOk (join `fmap` resp)