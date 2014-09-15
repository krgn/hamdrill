{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Users
       ( info
       , ping
       , senders) where

import Data.Aeson
import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

import qualified Data.ByteString.Lazy as LBS


-- | Return the information about the API-connected `User`
info :: (MonadIO m) => MandrillT m (Either ApiError User)
info = performRequest "/users/info.json" []


-- | Validate an API key and respond to a ping
ping :: ApiKey -> IO LBS.ByteString
ping key = 
     performRequest' "/users/ping.json" $
       encode $ object ["key" .= key]

-- | Return the senders that have tried to use this account, 
-- both verified and unverified
senders :: (MonadIO m) => MandrillT m (Either ApiError [Stat])
senders = performRequest "/users/senders.json" []
