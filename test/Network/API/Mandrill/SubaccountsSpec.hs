{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.SubaccountsSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text                    as Text 
import qualified Network.API.Mandrill.Subaccounts as Subaccounts
import           System.Environment


spec :: Spec
spec = do
  test_list
  test_add
  test_info
  test_update
  test_delete
  test_pause
  test_resume

test_resume :: Spec
test_resume = 
  describe "/subaccounts/resume.json" $
    it "should resume a paused subaccount" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        _ <- Subaccounts.add "acc-1" "My Acc" "yes, indeed." 50
        Subaccounts.resume "acc-1"
      resp `shouldSatisfy` isRight

test_pause :: Spec
test_pause = 
  describe "/subaccounts/pause.json" $
    it "should pause a subaccount" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        _ <- Subaccounts.add "acc-1" "My Acc" "yes, indeed." 50
        Subaccounts.pause "acc-1"
      resp `shouldSatisfy` isRight

test_delete :: Spec
test_delete = 
  describe "/subaccounts/delete.json" $
    it "should delete a subaccount" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Subaccounts.delete "acc-1"
      resp `shouldSatisfy` isRight

test_update :: Spec
test_update = 
  describe "/subaccounts/update.json" $
    it "update a subaccount" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Subaccounts.update "acc-1" "My Acc" "yes, indeed." 50
      resp `shouldSatisfy` isRight

test_info :: Spec
test_info = 
  describe "/subaccounts/info.json" $
    it "should return some info about a subaccount" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        _ <- Subaccounts.add "acc-1" "My Acc" "yes, indeed." 50
        Subaccounts.info "acc-1"
      resp `shouldSatisfy` isRight

test_add :: Spec
test_add = 
  describe "/subaccounts/add.json" $
    it "should add a subaccount" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Subaccounts.add "acc-1" "My Acc" "yes, indeed." 50
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/subaccounts/list.json" $
    it "should list all subaccounts" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Subaccounts.list "acc-1"
      resp `shouldSatisfy` isRight
