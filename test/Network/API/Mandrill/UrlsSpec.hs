{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.UrlsSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text              as Text 
import qualified Network.API.Mandrill.Urls  as Urls
import           System.Environment

spec :: Spec
spec = do
  test_list
  test_search
  test_timeSeries
  test_trackingDomains
  test_addTrackingDomain
  test_checkTrackingDomain 

test_list :: Spec
test_list = 
  describe "/urls/list.json" $
    it "should list all urls" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Urls.list
      resp `shouldSatisfy` isRight

test_search :: Spec
test_search = 
  describe "/urls/search.json" $
    it "should return a list of urls" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) (Urls.search "hello")
      resp `shouldSatisfy` isRight

test_timeSeries :: Spec
test_timeSeries = 
  describe "/urls/time-series.json" $
    it "should return a list of stats for an url" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
         Urls.timeSeries "example.com"
      resp `shouldSatisfy` isRight

test_trackingDomains :: Spec
test_trackingDomains = 
  describe "/urls/tracking-domains.json" $
    it "should return a list of tracking domains" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Urls.trackingDomains 
      resp `shouldSatisfy` isRight

test_addTrackingDomain :: Spec
test_addTrackingDomain = 
  describe "/urls/add-tracking-domain.json" $
    it "should add a tracking domain" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Urls.addTrackingDomain "example.com"
      resp `shouldSatisfy` isRight

test_checkTrackingDomain :: Spec
test_checkTrackingDomain = 
  describe "/urls/check-tracking-domain.json" $
    it "should check a tracking domain" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
         Urls.checkTrackingDomain "example.com" 
      resp `shouldSatisfy` isRight
