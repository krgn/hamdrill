{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.MetadataSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text                 as Text 
import qualified Network.API.Mandrill.Metadata as Metadata
import           System.Environment

spec :: Spec
spec = do
  test_list
  test_add
  test_update
  test_delete  

test_delete :: Spec
test_delete = 
  describe "/metadata/delete.json" $
    it "should delete a metadata entry" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Metadata.delete "meta-1"
      resp `shouldSatisfy` isRight

test_update :: Spec
test_update = 
  describe "/metadata/update.json" $
    it "should update a piece of metadata" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        let met = Metadata { _metadata_name = "hello"
                           , _metadata_state = STATE_active 
                           , _metadata_view_template = "bla"
                           }
            in Metadata.update met
      resp `shouldSatisfy` isRight

test_add :: Spec
test_add = 
  describe "/metadata/add.json" $
    it "should add a metadata field" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        let met = Metadata { _metadata_name = "hello"
                           , _metadata_state = STATE_active 
                           , _metadata_view_template = "bla"
                           }
          in Metadata.add met
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/metadata/list.json" $
    it "should list all metadata fields[" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Metadata.list
      resp `shouldSatisfy` isRight
