{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Utils 
       ( performRequest
       , performRequest' 
       , module Network.API.Mandrill.Monad
       , module Control.Monad.Reader
       ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import Control.Monad.Reader
import Network.HTTP.Conduit
import Network.API.Mandrill.Response
import Network.API.Mandrill.Monad
import Data.Aeson

-- | high-level function to query the Mandrill API give a `Value`, returning 
-- the type specified (must have an instance of the `MandrillResponse` class
-- in `Network.API.Mandrill.Response`
performRequest :: (MandrillResponse a, Monad m, MonadIO m) => 
                 String -> 
                 [(Text.Text,Value)] -> 
                 MandrillT m (Either ApiError a)
performRequest endpoint obj = do
  key <- ask
  response <- liftIO $ 
    performRequest' endpoint (payload key)
  liftIO $ print response
  return $ parseResponse response
  where payload k = (encode $ object $ ("key" .= k) : obj)

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

