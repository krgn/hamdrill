{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.InboundSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Utils
import qualified Data.Text                 as Text 
import qualified Network.API.Mandrill.Inbound as Inbound
import           System.Environment

spec :: Spec
spec = do
  test_domains
  test_addDomain
  test_checkDomain
  test_deleteDomain
  test_routes
  test_addRoute
  test_updateRoute
  test_deleteRoute
  test_sendRaw

test_sendRaw :: Spec
test_sendRaw = 
  describe "/inbound/send-raw.json" $
    it "should send a raw message" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let to = [ Recipient { _recipient_name  = "hello"
                           , _recipient_email = "hallo@hello.org"
                           , _recipient_type  = Nothing } ]
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.sendRaw "hello" to "hhallo@asd.org" "example.org" "123.123.123.123"
      resp `shouldSatisfy` isRight

test_deleteRoute :: Spec
test_deleteRoute = 
  describe "/inbound/delete-route.json" $
    it "should delete a route" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.deleteRoute "route-1"
      resp `shouldSatisfy` isRight

test_updateRoute :: Spec
test_updateRoute = 
  describe "/inbound/update-route.json" $
    it "should update a route" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.updateRoute "route-1" "mesaas-*" "examples.org"
      resp `shouldSatisfy` isRight

test_addRoute :: Spec
test_addRoute = 
  describe "/inbound/add-route.json" $
    it "should add a route" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.addRoute "example.com" "message-*" "examples.com"
      resp `shouldSatisfy` isRight

test_routes :: Spec
test_routes = 
  describe "/inbound/routes.json" $
    it "should list all routes" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.routes "example.com"
      resp `shouldSatisfy` isRight

test_deleteDomain :: Spec
test_deleteDomain = 
  describe "/inbound/delete-domain.json" $
    it "should delete an inbound domain" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.deleteDomain "example.com"
      resp `shouldSatisfy` isRight

test_checkDomain :: Spec
test_checkDomain = 
  describe "/inbound/check-domain.json" $
    it "should check an inbound domain" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.checkDomain "example.com"
      resp `shouldSatisfy` isRight

test_addDomain :: Spec
test_addDomain = 
  describe "/inbound/add-domain.json" $
    it "should add an inbound domain" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) $
        Inbound.addDomain "example.com"
      resp `shouldSatisfy` isRight

test_domains :: Spec
test_domains = 
  describe "/inbound/domains.json" $
    it "should list all inbound domains" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      resp <- runMandrill (ApiKey $ Text.pack raw) Inbound.domains
      resp `shouldSatisfy` isRight
