{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Response where

import Data.Aeson
import Data.API.JSON
import Data.Default
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy

class (FromJSONWithErrs r) => MandrillResponse r where
  parseResponse :: (FromJSONWithErrs r) => ByteString -> Either ApiError r

-- | We don't generate using api-tools this type like the others, as we'll need
-- it to parameterize our template splices with it and its instances
data ApiError = ApiError {
      status  :: String,
      code    :: Int,
      name    :: String,
      message :: String
    }
    deriving (Show)

-- | Default ApiError used in the MandrillResponse when returned JSON structure 
-- cannot be parsed.
instance Default ApiError where
  def = ApiError {
      status  = "error",
      code    = 3000,
      name    = "HS_Mandrill_Package_JSON_Parse_Error",
      message = ""
    }

instance FromJSON ApiError where
  parseJSON (Object v) = ApiError <$>
                         v .: "status" <*>
                         v .: "code" <*>
                         v .: "name" <*>
                         v .: "message" 
  parseJSON _          = mzero

instance FromJSONWithErrs ApiError where
  parseJSONWithErrs val = case fromJSON val of
                           Error e   -> failWith $ SyntaxError e
                           Success v -> pure v
