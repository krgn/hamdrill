{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Messages where

import Network.API.Mandrill.Utils
import Network.API.Mandrill.Response
import Network.API.Mandrill.Types


-- | Send a new transactional message through Mandrill
send :: (MonadIO m)   => 
       Message       -> 
       MessageConfig -> 
       MandrillT m (Either ApiError [DeliveryStatus])
send m c =
  performRequest "/messages/send.json" $
    [ "message" .= m 
    , "ip_pool" .= _conf_ip_pool c
    , "async"   .= _conf_async   c 
    , "send_at" .= _conf_send_at c ]


-- | Send a new transactional message through Mandrill using a template
sendTmpl :: (MonadIO m)    =>
           Message        -> 
           MessageConfig  -> 
           Name           -> 
           [Content]      -> 
           MandrillT m (Either ApiError [DeliveryStatus])
sendTmpl m c t tc =
  performRequest "/messages/send-template.json" $
    [ "template_name"    .= t
    , "template_content" .= tc
    , "message"          .= m
    , "ip_pool"          .= _conf_ip_pool  c
    , "async"            .= _conf_async    c 
    , "send_at"          .= _conf_send_at  c ]


-- | Search recently sent messages and optionally narrow by date 
-- range, tags, senders, and API keys. If no date range is specified, 
-- results within the last 7 days are returned. This method may be called 
-- up to 20 times per minute. If you need the data more often, 
-- you can use /messages/info.json to get the information for a single message, 
-- or webhooks to push activity to your own application for querying.
search :: (MonadIO m) =>
         Query       ->
         From        ->
         Thru        ->
         [Tag]       ->
         [Email]     ->
         [ApiKey]    -> 
         Count       ->
         MandrillT m (Either ApiError [SearchResult])
search q f t ts es ks c =
  performRequest "/messages/search.json" $
    [ "query"     .= q
    , "date_from" .= f
    , "date_to"   .= t
    , "tags"      .= ts
    , "senders"   .= es
    , "api_keys"  .= ks
    , "limit"     .= c ]


-- | Search the content of recently sent messages and return the aggregated 
-- hourly stats for matching messages
searchTimeSeries :: (MonadIO m ) =>
                   Query        ->
                   From         ->
                   Thru         ->
                   [Tag]        ->
                   [Email]      ->
                   MandrillT m (Either ApiError [Stat])
searchTimeSeries q f t ts es =
  performRequest "/messages/search-time-series.json" $
    [ "query"     .= q
    , "date_from" .= f
    , "date_to"   .= t
    , "tags"      .= ts
    , "senders"   .= es ]


-- | Get the information for a single recently sent message
info :: (MonadIO m) =>
       MessageId   -> 
       MandrillT m (Either ApiError SearchResult)
info mid = 
  performRequest "/messages/info.json" ["id" .= mid]


-- | Get the full content of a recently sent message
content :: (MonadIO m ) => 
          MessageId    -> 
          MandrillT m (Either ApiError Message)
content mid = 
  performRequest "/messages/content.json" ["id" .= mid]


-- | Parse the full MIME document for an email message, returning the content
-- of the message broken into its constituent pieces
parse :: (MonadIO m) => 
        RawMessage -> 
        MandrillT m (Either ApiError Message)
parse raw = 
  performRequest "/messages/parse.json" ["raw_message" .= raw]


-- | Take a raw MIME document for a message, and send it exactly as if it 
-- were sent through Mandrill's SMTP servers
sendRaw :: (MonadIO m)   =>
          RawMessage    -> 
          Email         -> 
          Name          -> 
          [Recipient]   -> 
          MessageConfig -> 
          MandrillT m (Either ApiError [DeliveryStatus])
sendRaw raw e n to c = 
  performRequest "/messages/send-raw.json" $
    [ "raw_message" .= raw
    , "from_email"  .= e
    , "from_name"   .= n
    , "to"          .= to
    , "async"       .= _conf_async c
    , "ip_pool"     .= _conf_ip_pool c
    , "send_at"     .= _conf_send_at c ]


-- | Queries your scheduled emails by sender or recipient, or both.
listScheduled :: (MonadIO m) =>
                Email       ->
                MandrillT m (Either ApiError [Scheduled])
listScheduled e = 
  performRequest "/messages/list-scheduled.json" ["to"  .= e]


-- | Cancels a scheduled email.
cancelScheduled :: (MonadIO m) =>
                  ScheduledId -> 
                  MandrillT m (Either ApiError Scheduled)
cancelScheduled i = 
  performRequest "/messages/cancel-scheduled.json" ["id"  .= i]


-- | Reschedules a scheduled email.
reschedule :: (MonadIO m) =>
             ScheduledId -> 
             TimeStamp   -> 
             MandrillT m (Either ApiError Scheduled)
reschedule i s =
  performRequest "/messages/reschedule.json" $
    [ "id"      .= i 
    , "send_at" .= s ]
