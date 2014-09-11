module Network.Mandrill.MessagesSpec where

import           Test.Hspec
import           Network.Mandrill.Types
import qualified Network.Mandrill.Messages as Messages
import qualified Data.Text as Text 


key :: ApiKey
key = ApiKey { _ApiKey =  Text.pack "b0c5wPDu1J9q_7MqPYAqBg" }


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
       let rcpt = TO_single Recipient 
                    { _recipient_email  = Text.pack "karsten@null2.net"
                    , _recipient_name = Text.pack "lotta luft"
                    , _recipient_type = Nothing
                    }

           msg = def { _msg_to      = rcpt 
                     , _msg_subject = Text.pack "test"
                     } :: Message

           cfg = MessageConfig {
                   _conf_async   = False
                 , _conf_ip_pool = Text.pack ""
                 , _conf_send_at = Text.pack ""
                 }

       response <- Messages.send key msg cfg
       case response of
         Left  e   -> status e   `shouldBe` "error"
         Right val -> length val `shouldBe` 1


test_sendTemplate :: Spec
test_sendTemplate = 
  describe "/messages/send-template.json" $
    it "should send a template message with valid key" $ do
       let rcpt = TO_single 
                     Recipient { _recipient_email = Text.pack "karsten@null2.net"
                               , _recipient_name  = Text.pack "lotta luft"
                               , _recipient_type  = Nothing }

           msg = def { _msg_to      = rcpt 
                     , _msg_subject = Text.pack "test"
                     } :: Message

           cfg = MessageConfig { _conf_async   = False
                               , _conf_ip_pool = Text.pack ""
                               , _conf_send_at = Text.pack "" }

           tmpl = Text.pack "test"

       response <- Messages.sendTmpl key msg cfg tmpl []
       case response of
         Left  e   -> status e   `shouldBe` "error"
         Right val -> length val `shouldBe` 1


test_search :: Spec
test_search = 
  describe "/messages/search.json" $
    it "should return some search results" $ do
       let q  = Text.pack "karsten@null2.net"
           f  = Text.pack "2013-01-01"
           t  = Text.pack "2013-01-03"
           ts = []
           ss = []
           ks = []
       resp <- Messages.search key q f t ts ss ks 10
       case resp of
         Left  e   -> status e       `shouldBe` "error"
         Right val -> length val == 0 `shouldBe` False


test_searchTimeSeries :: Spec
test_searchTimeSeries = 
  describe "/messages/search-time-series.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/messages/info.json" $
    it "" $ do
      --response <- info key
      pending

test_content :: Spec
test_content = 
  describe "/messages/content.json" $
    it "" $ do
      --response <- info key
      pending

test_parse :: Spec
test_parse = 
  describe "/messages/parse.json" $
    it "" $ do
      --response <- info key
      pending

test_sendRaw :: Spec
test_sendRaw = 
  describe "/messages/send-raw.json" $
    it "" $ do
      --response <- info key
      pending

test_listScheduled :: Spec
test_listScheduled = 
  describe "/messages/list-scheduled.json" $
    it "" $ do
      --response <- info key
      pending

test_cancelScheduled :: Spec
test_cancelScheduled = 
  describe "/messages/cancel-scheduled.json" $
    it "" $ do
      --response <- info key
      pending

test_reschedule :: Spec
test_reschedule = 
  describe "/messages/reschedule.json" $
    it "" $ do
      --response <- info key
      pending
