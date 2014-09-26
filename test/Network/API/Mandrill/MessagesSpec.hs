{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.MessagesSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib

import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Network.API.Mandrill.Messages as Messages
import qualified Network.API.Mandrill.Templates as Templates
import           System.Environment
import qualified Data.Text as Text 
import           Data.Time.Clock


spec :: Spec
spec = do
  test_send
  test_sendTemplate
  test_search
  test_searchTimeSeries
  test_info
  test_content
  test_parse
  test_sendRaw
  test_listScheduled
  test_cancelScheduled
  test_reschedule 


test_send :: Spec
test_send = 
  describe "/messages/send.json" $
    it "should send a message with valid key" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let rcpt = [ Recipient 
                   { _recipient_email = "karsten@null2.net"
                   , _recipient_name = "lotta luft"
                   , _recipient_type = Nothing
                   } ]

          msg = def { _msg_to      = Just rcpt 
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }

      resp <- runMandrill (ApiKey $ Text.pack raw) $ Messages.send msg cfg

      resp `shouldSatisfy` isRight


test_sendTemplate :: Spec
test_sendTemplate = 
  describe "/messages/send-template.json" $
    it "should send a template message with valid key" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let tmpl = def { _tmpl_name = "test" }
          rcpt =[ Recipient { _recipient_email = "karsten@null2.net"
                             , _recipient_name  = "lotta luft"
                             , _recipient_type  = Nothing } ]

          msg = def { _msg_to      = Just rcpt 
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig { _conf_async   = False
                              , _conf_ip_pool = ""
                              , _conf_send_at = Just TimeStamp { _ts_utctime = Just now } } 

      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        _ <- Templates.add tmpl True 
        Messages.sendTmpl msg cfg (_tmpl_name tmpl)  []
      resp `shouldSatisfy` isRight

test_search :: Spec
test_search = 
  describe "/messages/search.json" $
    it "should return some search results" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2013-01-03"
          ts = []
          ss = []
          ks = []
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        Messages.search q f t ts ss ks 10
      resp `shouldSatisfy` isRight

test_searchTimeSeries :: Spec
test_searchTimeSeries = 
  describe "/messages/search-time-series.json" $
    it "should return a list of statistics" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2013-01-03"
          ts = []
          ss = []
          ks = []
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        Messages.search q f t ts ss ks 10
      resp `shouldSatisfy` isRight


test_info :: Spec
test_info = 
  describe "/messages/info.json" $
    it "should return a message when queried" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2016-01-03"
          ts = []
          ss = []
          ks = []

      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
           query <- Messages.search q f t ts ss ks 10
           let vals = case query of
                       Right rs -> rs
                       Left _ -> []
           Messages.info (_result__id $ head vals)

      resp `shouldSatisfy` isRight


test_content :: Spec
test_content = 
  describe "/messages/content.json" $
    it "should return entire content of a sent message" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2016-01-03"
          ts = []
          ss = []
          ks = []
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        query <- Messages.search q f t ts ss ks 10
        let vals = case query of
                     Right rs -> rs
                     Left _ -> []
        Messages.content (_result__id $ head vals)
      resp `shouldSatisfy` isRight

test_parse :: Spec
test_parse = 
  describe "/messages/parse.json" $
    it "should return a message for our raw string" $ do
      raw <- getEnv "MANDRILL_API_KEY"

      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        Messages.parse "hello"
      resp `shouldSatisfy` isRight


test_sendRaw :: Spec
test_sendRaw = 
  describe "/messages/send-raw.json" $
    it "should send off a raw message" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let msg = "hahaha"
          email = "karsten@kurt.net"
          n    = "karsten kurt"
          to = [ Recipient { _recipient_email = "karsten@null2.net"
                           , _recipient_name  = "karste krut"
                           , _recipient_type  = Nothing } ]
          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        Messages.sendRaw msg email n to cfg
      resp `shouldSatisfy` isRight


test_listScheduled :: Spec
test_listScheduled = 
  describe "/messages/list-scheduled.json" $
    it "should list a scheduled email" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let email = "karsten@kurt.net"

          rcpt = [ Recipient 
                   { _recipient_email = "karsten@null2.net"
                   , _recipient_name = "lotta luft"
                   , _recipient_type = Nothing
                   } ]

          msg = def { _msg_to      = Just rcpt 
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        _ <- Messages.send msg cfg
        Messages.listScheduled email

      resp `shouldSatisfy` isRight
       

test_cancelScheduled :: Spec
test_cancelScheduled = 
  describe "/messages/cancel-scheduled.json" $
    it "should cancel a scheduled message" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let rcpt = [ Recipient 
                   { _recipient_email = "karsten@null2.net"
                   , _recipient_name = "lotta luft"
                   , _recipient_type = Nothing
                   } ]

          msg = def { _msg_to      = Just rcpt 
                    , _msg_from_email = Just "karsten@kurt.net"
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        d <- Messages.send msg cfg
        let st = case d of
                   Right v -> v
                   Left _  -> []
        Messages.cancelScheduled (_delivery__id $ head st)

      resp `shouldSatisfy` isRight

test_reschedule :: Spec
test_reschedule = 
  describe "/messages/reschedule.json" $
    it "should re-schedule a scheduled email" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        Messages.reschedule "user@host.com" TimeStamp { _ts_utctime = Just now }
      resp `shouldSatisfy` isRight
