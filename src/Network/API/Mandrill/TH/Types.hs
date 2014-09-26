{-# LANGUAGE QuasiQuotes #-}

module Network.API.Mandrill.TH.Types where

import Data.API.Parse
import Data.API.Types

mandrillApi :: API
mandrillApi = [api|

usr :: User
  // A record for a user
  = record 
    username     :: string
    created_at   :: ? TimeStamp
    public_id    :: ? string
    reputation   :: ? integer
    hourly_quota :: ? integer
    backlog      :: ? integer
    stats        :: ? Stats

stat :: Stat
  // a stats record 
  = record 
    time          :: ? string
    tag           :: ? Tag
    reputation    :: ? Count
    address       :: ? Email
    created_at    ::   TimeStamp
    sent          ::   Count
    hard_bounces  ::   Count
    soft_bounces  ::   Count
    rejects       ::   Count
    complaints    ::   Count
    unsubs        ::   Count
    opens         ::   Count
    unique_opens  ::   Count
    clicks        ::   Count
    unique_clicks ::   Count
    stats         :: ? Stats

stats :: Stats
  // A struct of stats by time
  = record
    today        :: Stat
    last_7_days  :: Stat
    last_30_days :: Stat
    last_60_days :: Stat
    last_90_days :: Stat
    all_time     :: Stat

sender :: Sender
  // a stats record 
  = record 
    address       :: Email
    created_at    :: TimeStamp
    sent          :: Count
    hard_bounces  :: Count
    soft_bounces  :: Count
    rejects       :: Count
    complaints    :: Count
    unsubs        :: Count
    opens         :: Count
    unique_opens  :: Count
    clicks        :: Count
    unique_clicks :: Count
    stats         :: ? Stats

metadata :: Metadata
  = record
    name          :: Name 
    state         :: MetadataState
    view_template :: string

rec :: Record
  // spf or dkim record info of a DomainRecord
  = record
    valid       :: boolean
    valid_after :: ? string
    error       :: ? string

domrec :: DomainRecord
  // a domain
  = record
    domain         :: Url
    created_at     :: TimeStamp
    last_tested_at :: TimeStamp
    spf            :: ? Record
    dkim           :: ? Record
    verified_at    :: ? TimeStamp
    valid_signing  :: boolean 

domstate :: DomainState
  = record 
    status :: DomainStatus
    domain :: Name
    email  :: Email

url :: UrlRecord 
  // an url record with click stats
  = record 
    url           :: ? Url
    time          :: ? string
    sent          :: Count
    clicks        :: Count
    unique_clicks :: Count

dnsinfo :: DnsInfo
  // dns info
  = record
    enabled :: ? boolean
    valid   :: ? boolean
    error   :: ? string

warmup :: Warmup
  // warmup
  = record
    warming_up :: boolean
    start_at   :: TimeStamp
    end_at     :: TimeStamp

ip :: IpRecord
  // an ip record 
  = record
    ip         :: IpAddress
    created_at :: ? TimeStamp
    pool       :: ? Name
    domain     :: ? Url
    custom_dns :: ? DnsInfo
    warmup     :: ? Warmup
    deleted    :: ? boolean

pool :: IpPool
  // a ip pool  record
  = record
    name       :: ? Name
    created_at :: ? TimeStamp
    ips        :: ? [IpRecord]
    deleted    :: ? boolean

provisioning :: IpProvision
  // a provisioning
  = record
    requested_at :: string

trackdom :: TrackingDomain 
  // a tracking domain
  = record 
    domain         :: Url 
    created_at     :: TimeStamp
    last_tested_at :: TimeStamp
    cname          :: Record
    valid_tracking :: boolean

tmpl :: Template
  // an email template
  = record 
    slug               :: ? string 
    name               ::   string
    labels             :: ? [Label]
    code               :: ? string
    subject            :: ? string
    from_email         :: ? Email
    from_name          :: ? string
    text               :: ? string 
    publish_name       :: ? string
    publish_code       :: ? string
    publish_subject    :: ? string
    publish_from_email :: ? Email
    publish_from_name  :: ? string
    publish_text       :: ? string
    published_at       :: ? TimeStamp
    content            :: ? string
    created_at         :: ? TimeStamp
    updated_at         :: ? TimeStamp

hook :: Webhook
  // a webhook
  = record 
    id           :: ? HookId
    url          :: Url
    description  :: Description
    auth_key     :: AuthKey
    events       :: [MessageEvent]
    created_at   :: TimeStamp
    last_sent_at :: ? TimeStamp
    batches_sent :: Count
    events_sent  :: Count
    last_error   :: ? string

acc  :: Subaccount
  // a subaccount
  = record
    id            :: ? SubaccountId
    name          :: Name
    custom_quota  :: Count
    hourly_quota  :: ? Count
    status        :: AccountStatus
    reputation    :: Count
    created_at    :: ? TimeStamp
    first_sent_at :: ? TimeStamp
    send_hourly   :: ? Count
    sent_weekly   :: Count 
    sent_monthly  :: Count
    sent_total    :: Count
    last_30_days  :: ? Stat 

indom :: InboundDomain
  // an inbound domain record
  = record 
    domain     :: Url
    created_at :: TimeStamp
    valid_mx   :: boolean

inroute :: InboundRoute
  // an inbound route
  = record 
    id      :: RouteId
    email   :: ? Email 
    pattern :: Pattern
    url     :: Url 

export :: Export
  // an export
  = record 
    id          :: ExportId
    created_at  :: TimeStamp
    finished_at :: ? TimeStamp
    type        :: ExportType
    state       :: ExportState
    result_url  :: ? Url

reject :: Reject 
  // a reject
  = record 
    email         :: Email
    reason        :: RejectReason
    detail        :: string
    created_at    :: TimeStamp
    last_event_at :: TimeStamp
    expires_at    :: TimeStamp
    expired       :: string 
    sender        :: Stat 
    subaccount    :: Subaccount

whitelist :: Whitelist
  // a whitelist entry
  = record 
    email      :: Email
    detail     :: ? string
    created_at :: ? TimeStamp

detail :: Detail
  // A stat containing information how often a messsage has been opened or stuff 
  // was clicked.
  = record 
    url      :: ? Url
    ip       :: IpAddress  
    location :: string
    ua       :: string

md :: MessageMetadata 
  // Message metadata
  = record 
    website :: ? string

rmd :: RecipientMetadata
  // per recipient metadata 
  = record
    rcpt   :: Email
    values :: MessageMetadata

mvar :: MergeVar
  // A merge var record
  = record
    name    :: string
    content :: string

rmvar :: RecipientMergeVar
  // bla
  = record
    recipient :: Email
    vars      :: [MergeVar] 

to :: To
  // a way to encode the possibility of a single or multiple recipient addresses 
  = union
    | single   :: Recipient
    | multiple :: [Recipient]

conf :: MessageConfig
  // config
  = record 
    async   :: Async
    ip_pool :: Name
    send_at :: ? TimeStamp

msg :: Message
  // A message structure
  = record 
    _id                       :: ? MessageId
    html                      :: ? string
    text                      :: ? string
    subject                   :: ? string
    from_email                :: ? Email
    from_name                 :: ? string
    to                        :: ? To
    important                 :: ? boolean
    track_opens               :: ? boolean
    track_clicks              :: ? boolean
    auto_text                 :: ? boolean
    auto_html                 :: ? boolean
    inline_css                :: ? boolean
    url_strip_qs              :: ? boolean
    preserve_recipients       :: ? boolean
    view_content_link         :: ? boolean
    bcc_address               :: ? Email
    tracking_domain           :: ? string
    signing_domain            :: ? string
    return_path_domain        :: ? string
    merge                     :: ? boolean
    global_merge_vars         :: ? [MergeVar]
    merge_vars                :: ? [RecipientMergeVar]
    tags                      :: ? [Tag]
    subaccount                :: ? string
    google_analytics_domains  :: ? [string]
    google_analytics_campaign :: ? string
    metadata                  :: ? MessageMetadata
    recipient_metadata        :: ? [RecipientMetadata]
    attachments               :: ? [Attachment]
    images                    :: ? [Image]

delivery :: DeliveryStatus
  // The response object when sending messages
  = record
    email         :: ? Email
    status        ::   SendStatus
    reject_reason :: ? RejectReason
    _id           ::   MessageId

result :: SearchResult
  // result type for search requests
  = record 
    _id           ::   MessageId
    sender        ::   Email
    template      :: ? Name
    subject       ::   string
    email         ::   Email
    tags          ::   [Tag]
    opens         ::   Count
    opens_detail  ::   [Detail]
    clicks        ::   Count
    clicks_detail ::   [Detail]
    state         ::   SendStatus
    metadata      ::   MessageMetadata
    smtp_events   :: ? [SmtpEvent]

event :: SmtpEvent
  // an event in the history of the message
  = record 
    type :: string
    diag :: string

sched :: Scheduled
  // a scheduled message
  = record
    _id        :: MessageId
    created_at :: TimeStamp
    send_at    :: TimeStamp
    from_email :: Email
    to         :: Email
    subject    :: string

|]
