{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.API.Mandrill.Types 
  ( module Network.API.Mandrill.Types
  , module Network.API.Mandrill.Response
  , module Data.Default
  , module Data.API.JSON
  , module Data.Aeson 
  ) where

import Data.Aeson hiding (withText, withBool)
import Data.Default
import Data.API.JSON
import Data.API.Tools
import Data.API.Tools.Mandrill
import Network.API.Mandrill.Response
import Network.API.Mandrill.TH.Utils
import Network.API.Mandrill.TH.Types
import Data.Maybe
import System.Locale
import Data.Time.Format
import Data.Time.Clock
import qualified Data.Text as Text

data TimeStamp = TimeStamp { _ts_utctime :: Maybe UTCTime }

instance Show TimeStamp where
  show t = concat [ "TimeStamp { _ts_utctime = "
                  , show $ _ts_utctime t ]

instance Eq TimeStamp where
  (==) t1 t2
      | _ts_utctime t1 == _ts_utctime t2 = True
      | otherwise = False
  (/=) t1 t2 = not (t1 == t2)


$(generate utils)

inj_TimeStamp :: REP__TimeStamp -> ParserWithErrs TimeStamp
inj_TimeStamp  (REP__TimeStamp as) = 
  return TimeStamp {  _ts_utctime = parse' as }
  where parse' = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" . Text.unpack 

prj_TimeStamp :: TimeStamp -> REP__TimeStamp
prj_TimeStamp mp | isJust (_ts_utctime mp) = REP__TimeStamp (Text.pack fts)
                 | otherwise = REP__TimeStamp ""
              where fts = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" $
                            fromJust $ _ts_utctime mp

$(generateAPITools utils [enumTool, jsonTool, lensTool, mandrillTool])


$(generate mandrillApi)
$(generateAPITools mandrillApi [enumTool, jsonTool, lensTool, mandrillTool])

instance Default Template where
  def = Template
        { _tmpl_slug               = Nothing
        , _tmpl_name               = ""
        , _tmpl_labels             = Nothing
        , _tmpl_code               = Nothing 
        , _tmpl_subject            = Nothing 
        , _tmpl_from_email         = Nothing 
        , _tmpl_from_name          = Nothing 
        , _tmpl_text               = Nothing  
        , _tmpl_publish_name       = Nothing 
        , _tmpl_publish_code       = Nothing 
        , _tmpl_publish_subject    = Nothing 
        , _tmpl_publish_from_email = Nothing 
        , _tmpl_publish_from_name  = Nothing 
        , _tmpl_publish_text       = Nothing 
        , _tmpl_published_at       = Nothing 
        , _tmpl_content            = Nothing 
        , _tmpl_created_at         = Nothing 
        , _tmpl_updated_at         = Nothing  
        }

instance Default Message where
  def = Message 
        { _msg__id                       = Nothing 
        , _msg_html                      = Nothing
        , _msg_text                      = Nothing
        , _msg_subject                   = Nothing
        , _msg_from_email                = Nothing
        , _msg_from_name                 = Nothing
        , _msg_to                        = Nothing
        , _msg_important                 = Just False
        , _msg_track_opens               = Nothing
        , _msg_track_clicks              = Nothing
        , _msg_auto_text                 = Nothing
        , _msg_auto_html                 = Nothing
        , _msg_inline_css                = Nothing
        , _msg_url_strip_qs              = Nothing
        , _msg_preserve_recipients       = Nothing
        , _msg_view_content_link         = Nothing
        , _msg_bcc_address               = Nothing
        , _msg_tracking_domain           = Nothing
        , _msg_signing_domain            = Nothing
        , _msg_return_path_domain        = Nothing
        , _msg_merge                     = Just True
        , _msg_global_merge_vars         = Just []
        , _msg_merge_vars                = Just []
        , _msg_tags                      = Just []
        , _msg_subaccount                = Nothing
        , _msg_google_analytics_domains  = Nothing
        , _msg_google_analytics_campaign = Nothing
        , _msg_metadata                  = Nothing
        , _msg_recipient_metadata        = Nothing
        , _msg_attachments               = Just []
        , _msg_images                    = Nothing
        }
