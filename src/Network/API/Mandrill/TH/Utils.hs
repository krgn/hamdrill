{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.API.Mandrill.TH.Utils where

import Data.API.Parse
import Data.API.Types

utils :: API
utils = [api|

cnt :: Count
  // a type alias for a count
  = integer

url :: Url
  // a type alias for an Url
  = string
  
ip :: IpAddress
  // a type alias for ip addresses
  = string

msgid :: MessageId
  // a type alias for message Ids
  = string

pblsh :: Publish 
  = boolean

asnc :: Async
  // type synonym for async sending
  = boolean

frm :: From
  // a date
  = string

thr :: Thru
  // a date
  = string

qry :: Query
  // a query
  = string

desc :: Description
  // a description
  = string

hid :: HookId
  = integer

cmt :: Comment
  // a comment
  = string

inexp :: IncExpired 
  // a flag
  = boolean

lbl :: Label
  // a template label
  = string

rawmsg :: RawMessage
  // a raw message string
  = string

sched :: ScheduledId
  // a scheduled id
  = string

sbcntid :: SubaccountId
  // a subaccount 
  = string

sbcntid :: RouteId
  // a route id
  = string

exptid :: ExportId
  // an export id
  = string

pttn :: Pattern
  // a pattern
  = string

clnadd :: ClientAddress
  // ip address
  = string

helo :: Helo
  // helo string
  = string

nts :: Notes
  // alias for name of soemthigng
  = string

nm :: Name 
  // alias for name of soemthigng
  = string

athk :: AuthKey
  // an auth key synonym
  = string

mlbx :: Mailbox
  = string

html :: HtmlFragment 
  = record
    html :: string

content :: Content
  // type synoym for the name of a template
  = record
    name    :: Name
    content :: string

status :: DomainStatus
  = enum
    | sent
    | already_verified

type :: ExportType
  // type of an export
  = enum 
    | activity
    | reject
    | whitelist

event :: MessageEvent
  // an event in the message lifecycle
  = enum 
    | send
    | hard_bounce
    | soft_bounce
    | open
    | click
    | spam
    | unsub
    | reject

state :: ExportState 
  // state of an export
  = enum 
    | waiting
    | working
    | complete
    | error
    | expired 

send_status :: SendStatus
  // The status enum 
  = enum
    | sent
    | queued
    | bounced
    | rejected
    | invalid

reason :: RejectReason
  // Reason enum when message was rejected
  = enum
    | hard_bounce
    | soft_bounce
    | spam
    | unsub
    | custom
    | invalid_sender
    | invalid 
    | test_mode_limit
    | rule 

state :: MetadataState
  // enum for status
  = enum
    | active
    | delete
    | index

account :: AccountStatus
  // enum for status
  = enum
    | active
    | paused

key :: ApiKey
  // key for api
  = basic string

tag :: Tag
  // a tag
  = string

email :: Email
  // Type synonym for email addresses
  = string

recipient :: Recipient
  // A recipient with data
  = record
    email :: Email
    name  :: Name
    type  :: ? HeaderType

header :: HeaderType
  // an enum to discern to cc and bcc header field types
  = enum
    | to
    | cc
    | bcc

image :: Image
  // A recpient of this email address
  = record
    email      :: Email
    name       :: Name
    headerType :: HeaderType

attachment :: Attachment 
  // An attachment to be added to the message 
  = record
    mimeType :: string
    name     :: Name
    content  :: string

stamp :: TimeStamp
  = basic string
  with inj_TimeStamp, prj_TimeStamp

|]
