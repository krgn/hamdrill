{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.IpsSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text              as Text 
import qualified Network.API.Mandrill.Ips   as Ips
import           System.Environment

spec :: Spec
spec = do
  test_list
  test_info
  test_provision
  test_startWarmup
  test_cancelWarmup
  test_setPool
  test_delete
  test_listPools
  test_poolInfo
  test_createPool
  test_deletePool
  test_checkCustomDns
  test_setCustomDns

test_setCustomDns :: Spec
test_setCustomDns = 
  describe "/ips/set-custom-dns.json" $
    it "should set a custom dns" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.setCustomDns "123.123.123.123" "example.com"
      resp `shouldSatisfy` isRight

test_checkCustomDns :: Spec
test_checkCustomDns = 
  describe "/ips/check-custom-dns.json" $
    it "should check custom dns" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.checkCustomDns "123.123.123.123" "example.com"
      resp `shouldSatisfy` isRight

test_createPool :: Spec
test_createPool = 
  describe "/ips/create-pool.json" $
    it "should create a pool" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
         Ips.createPool "pool-1"
      resp `shouldSatisfy` isRight

test_poolInfo :: Spec
test_poolInfo = 
  describe "/ips/pool-info.json" $
    it "should return some pool info" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
         Ips.poolInfo "pool-1"
      resp `shouldSatisfy` isRight

test_listPools :: Spec
test_listPools = 
  describe "/ips/list-pools.json" $
    it "should list all pools" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Ips.listPools
      resp `shouldSatisfy` isRight

test_deletePool :: Spec
test_deletePool = 
  describe "/ips/delete-pool.json" $
    it "should delete a pool" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.deletePool "pool-1"
      resp `shouldSatisfy` isRight

test_delete :: Spec
test_delete = 
  describe "/ips/delete.json" $
    it "should delete an ip" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.delete  "12.12.12.12"
      resp `shouldSatisfy` isRight

test_setPool :: Spec
test_setPool = 
  describe "/ips/set-pool.json" $
    it "should set pool of ip" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.setPool "12.12.12.12" "pool-1" False
      resp `shouldSatisfy` isRight

test_cancelWarmup :: Spec
test_cancelWarmup = 
  describe "/ips/cancel-warmup.json" $
    it "should cancel ip warmup" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.cancelWarmup "12.12.12.12"
      resp `shouldSatisfy` isRight

test_startWarmup :: Spec
test_startWarmup = 
  describe "/ips/start-warmup.json" $
    it "should start warmup of ip" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.startWarmup "12.12.12.12"
      resp `shouldSatisfy` isRight

test_provision :: Spec
test_provision = 
  describe "/ips/provision.json" $
    it "should provision an ip" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.provision False "pool-1"
      resp `shouldSatisfy` isRight

test_info :: Spec
test_info = 
  describe "/ips/info.json" $
    it "should show some info for ip" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Ips.info  "12.12.12.12"
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/ips/list.json" $
    it "should list all ips" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Ips.list
      resp `shouldSatisfy` isRight
