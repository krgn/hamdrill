{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Mandrill.Types 
  ( module Network.Mandrill.Types
  , module Network.Mandrill.Response
  , module Data.Default
  , module Data.API.JSON
  , module Data.Aeson 
  ) where

import Data.Aeson hiding (withText, withBool)
import Data.Default
import Data.API.JSON
import Data.API.Tools
import Data.API.Tools.Mandrill
import Network.Mandrill.Response
import Network.Mandrill.TH.Utils
import Network.Mandrill.TH.Types
import System.Locale
import Data.Time.Format
import Data.Time.Clock
import qualified Data.Text as Text

-- splice in all our data type definitions
$(generate utils)

-- created_at 
-- parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" a

type TimeStamp = Maybe UTCTime
-- 
inj_TimeStamp :: REP__TimeStamp -> ParserWithErrs TimeStamp
inj_TimeStamp  (REP__TimeStamp as) = 
  return $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" (Text.unpack as)

prj_TimeStamp :: TimeStamp -> REP__TimeStamp
prj_TimeStamp (Just mp) = REP__TimeStamp $ 
  Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" mp
prj_TimeStamp  Nothing   = REP__TimeStamp $ 
  Text.pack ""

$(generateAPITools utils [enumTool, jsonTool, mandrillTool])


$(generate mandrillApi)
$(generateAPITools mandrillApi [enumTool, jsonTool, mandrillTool])
	
	
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
        , _msg_ts                        = Nothing 
        , _msg_html                      = "<strong>test</strong>"
        , _msg_text                      = "test"
        , _msg_subject                   = "this is a .. "
        , _msg_from_email                = "karsten@null2.net"
        , _msg_from_name                 = "Karsten Gebbert"
        , _msg_to                        = TO_single
          Recipient
            { _recipient_email           = "torsten@null2.net"
            , _recipient_name            = "T. Orsten"
            , _recipient_type            = Nothing
            }
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
        , _msg_tags                      = []
        , _msg_subaccount                = Nothing
        , _msg_google_analytics_domains  = Nothing
        , _msg_google_analytics_campaign = Nothing
        , _msg_metadata                  = Nothing
        , _msg_recipient_metadata        = Nothing
        , _msg_attachments               = []
        , _msg_images                    = Nothing
        }
