{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.SendersSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text                as Text 
import qualified Network.API.Mandrill.Senders as Senders
import           System.Environment

spec :: Spec
spec = do
  test_list
  test_domains
  test_addDomain
  test_checkDomain
  test_info
  test_timeSeries

test_list :: Spec
test_list = 
  describe "/senders/list.json" $
    it "should list all senders" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Senders.list
      resp `shouldSatisfy` isRight

test_domains :: Spec
test_domains = 
  describe "/senders/domains.json" $
    it "should list all domains" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Senders.domains 
      resp `shouldSatisfy` isRight

test_addDomain :: Spec
test_addDomain = 
  describe "/senders/add-domain.json" $
    it "should add a domain" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ 
        Senders.addDomain "foo.org"
      resp `shouldSatisfy` isRight

test_checkDomain :: Spec
test_checkDomain = 
  describe "/senders/check-domain.json" $
    it "should ackknowledge a domain check process" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ 
        Senders.checkDomain "foo.org"
      resp `shouldSatisfy` isRight

test_info :: Spec
test_info = 
  describe "/senders/info.json" $
    it "should show detailed sender info/stats" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ 
        Senders.info "baz@foo.org"
      resp `shouldSatisfy` isRight

test_timeSeries :: Spec
test_timeSeries = 
  describe "/senders/time-series.json" $
    it "should return stats for a sender" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ 
        Senders.timeSeries  "baz@foo.org"
      resp `shouldSatisfy` isRight
