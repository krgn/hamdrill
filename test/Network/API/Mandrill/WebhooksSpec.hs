{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.WebhooksSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text                 as Text 
import qualified Network.API.Mandrill.Webhooks as Webhooks
import           System.Environment
import Data.Maybe

spec :: Spec
spec = do
  test_list
  test_add
  test_info
  test_update
  test_delete

test_delete :: Spec
test_delete = 
  describe "/webhooks/delete.json" $
    it "delete a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        a <- Webhooks.add "example.com" "it is what it is" []
        let hook = case a of
                     Right h -> h
                     Left _ -> undefined
         in Webhooks.delete (fromJust $ _hook_id hook)
      resp `shouldSatisfy` isRight

test_update :: Spec
test_update = 
  describe "/webhooks/update.json" $
    it "should update a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        a <- Webhooks.add "example.com" "it is what it is" []
        let hook = case a of
                     Right h -> h
                     Left _ -> undefined
         in Webhooks.update (fromJust $ _hook_id hook) "example.com" "it is what it is" []
      resp `shouldSatisfy` isRight

test_info :: Spec
test_info = 
  describe "/webhooks/info.json" $
    it "should return some detailed info about a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $ do
        a <- Webhooks.add "example.com" "it is what it is" []
        let hook = case a of
                     Right h -> h
                     Left _ -> undefined
         in Webhooks.info (fromJust $ _hook_id hook)
      resp `shouldSatisfy` isRight

test_add :: Spec
test_add = 
  describe "/webhooks/add.json" $
    it "should add a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Webhooks.add "example.com" "it is what it is" []
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/webhooks/list.json" $
    it "should list all webhooks" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Webhooks.list
      resp `shouldSatisfy` isRight
