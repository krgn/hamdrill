{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Senders where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | Return the senders that have tried to use this account.
list :: (MonadIO m) => MandrillT m (Either ApiError [Sender])
list = performRequest "/senders/list.json" []


-- | Returns the sender domains that have been added to this account.
domains :: (MonadIO m) => MandrillT m (Either ApiError [DomainRecord])
domains = performRequest "/senders/domains.json" []


-- | Adds a sender domain to your account. Sender domains are added 
-- automatically as you send, but you can use this call to add them ahead 
-- of time.
addDomain :: (MonadIO m) => 
            Name -> 
            MandrillT m (Either ApiError DomainRecord)
addDomain n = performRequest "/senders/add-domain.json" [ "domain" .= n]


-- | Checks the SPF and DKIM settings for a domain. If you haven't already 
-- added this domain to your account, it will be added automatically.
checkDomain :: (MonadIO m) =>
              Name ->
              MandrillT m (Either ApiError DomainRecord)
checkDomain n = performRequest "/senders/check-domain.json" ["domain" .= n]


-- | Sends a verification email in order to verify ownership of a domain. 
-- Domain verification is an optional step to confirm ownership of a domain. 
-- Once a domain has been verified in a Mandrill account, other accounts may 
-- not have their messages signed by that domain unless they also verify the 
-- domain. This prevents other Mandrill accounts from sending mail signed by 
-- your domain.
verifyDomain :: (MonadIO m) => 
               Name -> 
               Mailbox ->
               MandrillT m (Either ApiError DomainState)
verifyDomain n m = performRequest "/senders/verify-domain.json" $ 
                     [ "domain"  .= n
                     , "mailbox" .= m ]


-- | Return more detailed information about a single sender, including 
-- aggregates of recent stats
info :: (MonadIO m) =>
       Email -> 
       MandrillT m (Either ApiError Stat)
info e = performRequest "/senders/info.json" ["address" .= e]


-- | Return the recent history (hourly stats for the last 30 days) for a sender
timeSeries :: (MonadIO m) => 
             Email -> 
             MandrillT m (Either ApiError [Stat])
timeSeries e = performRequest "/senders/time-series.json" ["address" .= e]


