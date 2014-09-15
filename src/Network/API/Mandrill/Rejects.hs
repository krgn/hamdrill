{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Rejects where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | Adds an email to your email rejection blacklist. Addresses that you add 
-- manually will never expire and there is no reputation penalty for removing 
-- them from your blacklist. Attempting to blacklist an address that 
-- has been whitelisted will have no effect.
add :: (MonadIO m) =>
      Email        -> 
      Comment      -> 
      SubaccountId ->  
      MandrillT m (Either ApiError Reject)
add e c a =
  performRequest "/rejects/add.json" $
    [ "email"      .= e 
    , "comment"    .= c 
    , "subaccount" .= a ]

-- | Retrieves your email rejection blacklist. You can provide an email 
-- address to limit the results. Returns up to 1000 results. By default, 
-- entries that have expired are excluded from the results; set 
-- `IncExpired` to true to include them.
list :: (MonadIO m)  =>
       Email        -> 
       IncExpired   -> 
       SubaccountId -> 
       MandrillT m (Either ApiError Reject)
list e i s =
  performRequest "/rejects/list.json" $
    [ "email"           .= e
    , "include_expired" .= i 
    , "subaccount"      .= s ]

-- | Deletes an email rejection. There is no limit to how many rejections 
-- you can remove from your blacklist, but keep in mind that each deletion 
-- has an affect on your reputation.
delete :: (MonadIO m)  =>
         Email        -> 
         SubaccountId -> 
         MandrillT m (Either ApiError Reject)
delete e s = performRequest "/rejects/delete.json" $
               [ "email"      .= e
               , "subaccount" .= s]
