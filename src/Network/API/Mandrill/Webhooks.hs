{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Webhooks where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | Get the list of all webhooks defined on the account
list :: (MonadIO m) => MandrillT m (Either ApiError [Webhook])
list = performRequest "/webhooks/list.json" []

-- | Add a new webhook
add :: (MonadIO m) =>
      Url -> 
      Description -> 
      [MessageEvent] -> 
      MandrillT m (Either ApiError Webhook)
add u d es = 
  performRequest "/webhooks/add.json" $
           [ "url"         .= u
           , "description" .= d 
           , "events"      .= es ]

-- | Given the ID of an existing webhook, return the data about it
info :: (MonadIO m) => 
       HookId -> 
       MandrillT m (Either ApiError Webhook)
info i = 
  performRequest "/webhooks/info.json" [ "id" .= i ]

-- | Update an existing webhook
update :: (MonadIO m) =>
         HookId -> 
         Url ->
         Description ->
         [MessageEvent] ->
         MandrillT m (Either ApiError Webhook)
update i u d es =
  performRequest "/webhooks/update.json" $
           [ "id"          .= i
           , "url"         .= u
           , "description" .= d 
           , "events"      .= es ]

-- | Delete an existing webhook
delete :: (MonadIO m) =>
         HookId -> 
         MandrillT m (Either ApiError Webhook)
delete i = performRequest "/webhooks/delete.json" ["id"  .= i]


