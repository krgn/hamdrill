{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Whitelists where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils


-- | Adds an email to your email rejection whitelist. If the address is 
-- currently on your blacklist, that blacklist entry will be removed 
-- automatically.
add :: (MonadIO m) =>
      Email       -> 
      Comment     -> 
      MandrillT m (Either ApiError Whitelist)
add e c =
    performRequest "/whitelists/add.json" $
             [ "email"   .= e 
             , "comment" .= c]


-- | Retrieves your email rejection whitelist. You can provide an email address 
-- or search prefix to limit the results. Returns up to 1000 results.
list :: (MonadIO m) =>
        Email      -> 
        MandrillT m (Either ApiError [Whitelist])
list e = performRequest "/whitelists/list.json" ["email" .= e]


-- | Removes an email address from the whitelist.
delete :: (MonadIO m) =>
          Email      -> 
          MandrillT m (Either ApiError Whitelist) 
delete e = performRequest "/whitelists/delete.json" ["email" .= e]
