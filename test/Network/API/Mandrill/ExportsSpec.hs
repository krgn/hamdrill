{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.ExportsSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text                as Text 
import qualified Network.API.Mandrill.Exports as Exports
import           System.Environment


spec :: Spec
spec = do
  test_info
  test_list
  test_rejects
  test_whitelist
  test_activity

test_activity :: Spec
test_activity = 
  describe "/exports/activity.json" $
    it "should show activity stats" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Exports.activity "user@host.com" "2014-02-01" "2014-02-01" [] [] [] []
      resp `shouldSatisfy` isRight

test_whitelist :: Spec
test_whitelist = 
  describe "/exports/whitelist.json" $
    it "should export whitelists" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Exports.whitelist "user@host.com"
      resp `shouldSatisfy` isRight

test_rejects :: Spec
test_rejects = 
  describe "/exports/rejects.json" $
    it "should export rejets" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Exports.rejects "user@host.com"
      resp `shouldSatisfy` isRight

test_info :: Spec
test_info = 
  describe "/exports/info.json" $
    it "should show some exports info" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Exports.info "e-1"
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/exports/list.json" $
    it "should list all exports" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Exports.list 
      resp `shouldSatisfy` isRight
