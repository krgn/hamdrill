{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Exports where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | Returns a list of your exports.
list :: (MonadIO m) => MandrillT m (Either ApiError [Export])
list = performRequest "/exports/list.json" []

-- |  Returns information about an export job. If the export job's state is 
-- 'complete', the returned data will include a URL you can use to fetch the 
-- results. Every export job produces a zip archive, but the format of the 
-- archive is distinct for each job type. The api calls that initiate exports 
-- include more details about the output format for that job type.
info :: (MonadIO m) => 
       ExportId -> 
       MandrillT m (Either ApiError Export)
info i = performRequest "/exports/info.json" [ "id" .= i ]


-- | Begins an export of your rejection blacklist. The blacklist will be 
-- exported to a zip archive containing a single file named rejects.csv that 
-- includes the following fields: email, reason, detail, created_at, 
-- expires_at, last_event_at, expires_at.
rejects :: (MonadIO m) => 
          Email -> 
          MandrillT m (Either ApiError Export)
rejects e = performRequest "/exports/rejects.json" [ "notify_email" .= e ]


-- | Begins an export of your rejection whitelist. The whitelist will be 
-- exported to a zip archive containing a single file named whitelist.csv 
-- that includes the following fields: email, detail, created_at.
whitelist :: (MonadIO m) => 
            Email -> 
            MandrillT m (Either ApiError Export)
whitelist e = performRequest "/exports/whitelist.json" [ "notify_email" .= e ]


-- | Begins an export of your activity history. The activity will be exported 
-- to a zip archive containing a single file named activity.csv in the same 
-- format as you would be able to export from your account's activity view. It 
-- includes the following fields: Date, Email Address, Sender, Subject, Status, 
-- Tags, Opens, Clicks, Bounce Detail. If you have configured any custom metadata 
-- fields, they will be included in the exported data.
activity :: (MonadIO m) =>
           Email -> 
           From -> 
           Thru -> 
           [Tag] -> 
           [Email] -> 
           [SendStatus] -> 
           [ApiKey] -> 
           MandrillT m (Either ApiError Export)
activity e f t ts ss sts ks = 
  performRequest "/exports/activity.json" $
           [ "notify_email" .= e
           , "date_from"    .= f
           , "date_to"      .= t
           , "tags"         .= ts
           , "senders"      .= ss
           , "states"       .= sts
           , "api_keys"     .= ks ]

