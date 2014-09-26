{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Subaccounts where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Utils
import Network.API.Mandrill.Types


-- | Get the list of subaccounts defined for the account, 
-- optionally filtered by a prefix
list :: (MonadIO m) =>
       Maybe Query -> 
       MandrillT m (Either ApiError [Subaccount])
list q = performRequest "/subaccounts/list.json" [ "q" .= q ]


-- | Add a new subaccount
add :: (MonadIO m) =>
      SubaccountId -> 
      Name ->
      Notes -> 
      Count -> 
      MandrillT m (Either ApiError Subaccount)
add s na no c =
    performRequest "/subaccounts/add.json" $
             [ "id"           .= s
             , "name"         .= na
             , "notes"        .= no
             , "custom_quota" .= c ]


-- | Given the ID of an existing subaccount, return the data about it
info :: (MonadIO m) =>
       SubaccountId -> 
       MandrillT m (Either ApiError Subaccount)
info i = performRequest "/subaccounts/info.json" [ "id" .= i ]


-- | Update an existing subaccount
update :: (MonadIO m) =>
         SubaccountId -> 
         Name -> 
         Notes -> 
         Count -> 
         MandrillT m (Either ApiError Subaccount)
update s na no c = 
  performRequest "/subaccounts/update.json" $
          [ "id"           .= s
          , "name"         .= na
          , "notes"        .= no
          , "custom_quota" .= c ]


-- | Delete an existing subaccount. Any email related to the subaccount will be 
-- saved, but stats will be removed and any future sending calls to this 
-- subaccount will fail.
delete :: (MonadIO m) => 
         SubaccountId -> 
         MandrillT m (Either ApiError Subaccount)
delete i = performRequest "/subaccounts/delete.json" [ "id" .= i ] 

-- | Pause a subaccount's sending. Any future emails delivered to this 
-- subaccount will be queued for a maximum of 3 days until the subaccount 
-- is resumed.
pause :: (MonadIO m) => 
        SubaccountId -> 
        MandrillT m (Either ApiError Subaccount)
pause i = performRequest "/subaccounts/pause.json" [ "id" .= i ]


-- | Resume a paused subaccount's sending
resume :: (MonadIO m) => 
         SubaccountId -> 
         MandrillT m (Either ApiError Subaccount)
resume i = performRequest "/subaccounts/resume.json" [ "id" .= i ]
