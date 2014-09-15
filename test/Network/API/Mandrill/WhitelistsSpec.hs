{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.WhitelistsSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text                   as Text 
import qualified Network.API.Mandrill.Whitelists as Whitelist
import           System.Environment

spec :: Spec
spec = do
  test_add
  test_list
  test_delete

test_add :: Spec
test_add = 
  describe "/whitelists/add.json" $
    it "should add something to the whitelists" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Whitelist.add "cow@moo.org" "bounces"
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/whitelists/list.json" $
    it "should list all entries on the whitelist" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
         Whitelist.list "cow@moo.org"
      resp `shouldSatisfy` isRight

test_delete :: Spec
test_delete = 
  describe "/whitelists/delete.json" $
    it "should delete an item from the whitelists" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Whitelist.delete "cow@moo.org"
      resp `shouldSatisfy` isRight
