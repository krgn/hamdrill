{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Ips where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Utils
import Network.API.Mandrill.Types

-- | Lists your dedicated IPs.
list :: (MonadIO m) => MandrillT m (Either ApiError [IpRecord])
list = performRequest "/ips/list.json" [] 


-- | Retrieves information about a single dedicated ip.
info :: (MonadIO m) =>
       IpAddress ->
       MandrillT m (Either ApiError [IpRecord])
info i = performRequest "/ips/info.json" [ "ip" .= i ] 


-- | Requests an additional dedicated IP for your account. Accounts may have 
-- one outstanding request at any time, and provisioning requests are processed 
-- within 24 hours.
provision :: (MonadIO m) =>
            Bool ->
            Name ->
            MandrillT m (Either ApiError IpProvision)
provision w p =
  performRequest "/ips/provision.json" $
          [ "warmup" .= w
          , "pool"   .= p ] 


-- | Begins the warmup process for a dedicated IP. During the warmup process, 
-- Mandrill will gradually increase the percentage of your mail that is sent 
-- over the warming-up IP, over a period of roughly 30 days. The rest of your 
-- mail will be sent over shared IPs or other dedicated IPs in the same pool.
startWarmup :: (MonadIO m) =>
              IpAddress ->
              MandrillT m (Either ApiError IpRecord)
startWarmup i = performRequest "/ips/start-warmup.json" [ "ip" .= i ] 


-- | Cancels the warmup process for a dedicated IP.
cancelWarmup :: (MonadIO m) =>
                IpAddress ->
                MandrillT m (Either ApiError IpRecord)
cancelWarmup i = performRequest "/ips/cancel-warmup.json" [ "ip" .= i ] 

-- | Moves a dedicated IP to a different pool.
setPool :: (MonadIO m) =>
           IpAddress ->
           Name ->
           Bool ->
           MandrillT m (Either ApiError IpRecord)
setPool i p c = 
        performRequest "/ips/cancel-warmup.json" $
                 [ "ip"          .= i
                 , "create_pool" .= c
                 , "pool"        .= p ] 


-- | Deletes a dedicated IP. This is permanent and cannot be undone.
delete :: (MonadIO m) =>
          IpAddress ->
          MandrillT m (Either ApiError IpRecord)
delete i = performRequest "/ips/delete.json" [ "ip" .= i ] 


-- | Lists your dedicated IP pools.
listPools :: (MonadIO m) => MandrillT m (Either ApiError [IpPool])
listPools = performRequest "/ips/list-pools.json" [] 


-- | Describes a single dedicated IP pool.
poolInfo :: (MonadIO m) =>
            Name ->
            MandrillT m (Either ApiError IpPool)
poolInfo p = performRequest "/ips/pool-info.json" [ "pool" .= p ] 


-- | Creates a pool and returns it. If a pool already exists with this 
-- name, no action will be performed.
createPool :: (MonadIO m) =>
             Name ->
             MandrillT m (Either ApiError IpPool)
createPool p = performRequest "/ips/create-pool.json" [ "pool" .= p ] 


-- | Deletes a pool. A pool must be empty before you can delete it, and you 
-- cannot delete your default pool.
deletePool :: (MonadIO m) =>
             Name ->
             MandrillT m (Either ApiError IpPool)
deletePool p = performRequest "/ips/delete-pool.json" [ "pool" .= p ] 


-- | Tests whether a domain name is valid for use as the custom 
-- reverse DNS for a dedicated IP.
checkCustomDns :: (MonadIO m) =>
                 IpAddress ->
                 Url ->
                 MandrillT m (Either ApiError DnsInfo)
checkCustomDns i u = performRequest "/ips/check-custom-dns.json" $
                        [ "ip"     .= i
                        , "domain" .= u ] 


-- | Configures the custom DNS name for a dedicated IP.
setCustomDns :: (MonadIO m) =>
                IpAddress ->
                Url ->
                MandrillT m (Either ApiError IpRecord)
setCustomDns i u = performRequest "/ips/set-custom-dns.json" $
                      [ "ip"     .= i
                      , "domain" .= u ] 

