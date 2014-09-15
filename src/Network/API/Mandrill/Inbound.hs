{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Inbound where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | List the domains that have been configured for inbound delivery
domains :: (MonadIO m) => MandrillT m (Either ApiError [InboundDomain])
domains = performRequest "/inbound/domains.json" []


-- | Add an inbound domain to your account
addDomain :: (MonadIO m) =>
            Url -> 
            MandrillT m (Either ApiError InboundDomain)
addDomain d = performRequest "/inbound/add-domain.json" [ "domain" .= d ]


-- | Check the MX settings for an inbound domain. The domain must have 
-- already been added with the add-domain call
checkDomain :: (MonadIO m) => 
              Url -> 
              MandrillT m (Either ApiError InboundDomain)
checkDomain d = performRequest "/inbound/check-domain.json" [ "domain" .= d ]


-- | Delete an inbound domain from the account. All mail will stop routing 
-- for this domain immediately.
deleteDomain :: (MonadIO m) =>
               Url -> 
               MandrillT m (Either ApiError InboundDomain)
deleteDomain d = performRequest "/inbound/delete-domain.json" [ "domain" .= d ]


-- | List the mailbox routes defined for an inbound domain
routes :: (MonadIO m) => 
         Url -> 
         MandrillT m (Either ApiError [InboundRoute])
routes d = performRequest "/inbound/routes.json" [ "domain" .= d ]


-- | Add a new mailbox route to an inbound domain
addRoute :: (MonadIO m) => 
           Url -> 
           Pattern -> 
           Url -> 
           MandrillT m (Either ApiError InboundRoute)
addRoute u1 p u2 =
  performRequest "/inbound/add-route.json" $
           [ "domain"  .= u1
           , "pattern" .= p
           , "url"     .= u2 ]


-- | Update the pattern or webhook of an existing inbound mailbox route. 
-- If `Nothing` is provided for any fields, the values will remain unchanged.
updateRoute :: (MonadIO m) =>
              RouteId -> 
              Pattern -> 
              Url -> 
              MandrillT m (Either ApiError InboundRoute)
updateRoute i p u =
  performRequest "/inbound/update-route.json" $
           [ "id"      .= i
           , "pattern" .= p
           , "url"     .= u ]


-- | Delete an existing inbound mailbox route
deleteRoute :: (MonadIO m) => 
              RouteId -> 
              MandrillT m (Either ApiError InboundRoute)
deleteRoute i = performRequest "/inbound/delete-route.json" [ "id" .= i ]


-- | Take a raw MIME document destined for a domain with inbound domains set 
-- up, and send it to the inbound hook exactly as if it had been sent over SMTP
sendRaw :: (MonadIO m) =>
          RawMessage -> 
          To -> 
          Email -> 
          Helo -> 
          ClientAddress -> 
          MandrillT m (Either ApiError [InboundRoute])
sendRaw r t e h a =
  performRequest "/inbound/send-raw.json" $
           [ "raw_message"    .= r
           , "to"             .= t
           , "mail_from"      .= e
           , "helo"           .= h
           , "client_address" .= a ]

