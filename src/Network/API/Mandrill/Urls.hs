{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Urls where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | Get the 100 most clicked URLs
list :: (MonadIO m) => MandrillT m (Either ApiError [UrlRecord])
list = performRequest "/urls/list.json" []


-- | Return the 100 most clicked URLs that match the search query given
search :: (MonadIO m) =>
         Query -> 
         MandrillT m (Either ApiError [UrlRecord])
search q = performRequest "/urls/search.json" ["q" .= q]


-- | Return the recent history (hourly stats for the last 30 days) for a url
timeSeries :: (MonadIO m) =>
             Url -> 
             MandrillT m (Either ApiError [Stat])
timeSeries u = performRequest "/urls/time-series.json" ["url" .= u]


-- | Get the list of tracking domains set up for this account
trackingDomains :: (MonadIO m) => MandrillT m (Either ApiError [TrackingDomain])
trackingDomains = performRequest "/urls/tracking-domains.json" []


-- | Add a tracking domain to your account
addTrackingDomain :: (MonadIO m) =>
                    Url -> 
                    MandrillT m (Either ApiError TrackingDomain)
addTrackingDomain d =
  performRequest "/urls/add-tracking-domain.json" [ "domain" .= d ]


-- | Checks the CNAME settings for a tracking domain. The domain must have been 
-- added already with the add-tracking-domain call
checkTrackingDomain :: (MonadIO m) =>
                      Url -> 
                      MandrillT m (Either ApiError TrackingDomain)
checkTrackingDomain d =
  performRequest "/urls/add-tracking-domain.json" [ "domain" .= d ]
