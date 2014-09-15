module Network.API.Mandrill.UsersSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text              as Text 
import qualified Network.API.Mandrill.Users as Users
import qualified Data.ByteString.Char8  as CBS
import qualified Data.ByteString.Lazy   as LBS
import           System.Environment


spec :: Spec
spec = do
  test_info
  test_ping
  test_senders


test_info :: Spec
test_info = 
  describe "/users/info.json" $
    it "should return some user info upon valid request" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      response <- runMandrill (ApiKey $ Text.pack raw) Users.info
      response `shouldSatisfy` isRight


test_ping :: Spec
test_ping = 
  describe "/users/ping.json" $
    it "should return pong upon valid request" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      response <- Users.ping key
      CBS.unpack (LBS.toStrict response) `shouldBe` "\"PONG!\""


test_senders :: Spec
test_senders = 
  describe "/users/senders.json" $
    it "should return a list of sender that have used this account" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      response <- runMandrill (ApiKey $ Text.pack raw) Users.senders
      response `shouldSatisfy` isRight
