{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Utils 
       ( performRequest
       , performRequest' ) where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import Network.Mandrill.Response
import Data.Aeson

-- | high-level function to query the Mandrill API give a `Value`, returning 
-- the type specified (must have an instance of the `MandrillResponse` class
-- in `Network.Mandrill.Response`
performRequest :: (MandrillResponse a) => String -> Value -> IO (Either ApiError a)
performRequest endpoint obj = do
               response <- performRequest' endpoint (encode obj)
               print response
               let parsed = parseResponse response
               return parsed

-- | lower-level version 
performRequest' :: String -> LBS.ByteString -> IO LBS.ByteString
performRequest' endpoint obj = do
  initReq <- parseUrl (baseUrl ++ endpoint) 

  let request = initReq {
        method = "POST",
        checkStatus = \ _ _ _ -> Nothing,
        requestBody = RequestBodyLBS obj
      }

  withManager $ \ manager -> do
    resp <- httpLbs request manager
    return $ responseBody resp

  where baseUrl = "https://mandrillapp.com/api/1.0"

