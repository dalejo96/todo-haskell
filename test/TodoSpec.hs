{-# LANGUAGE OverloadedStrings #-}

module TodoSpec where

-- directory
import System.Directory (removeFile)

-- hspec
import Test.Hspec

-- shortener
import Todo (todoApp)

-- wai
import Network.Wai

-- wai-extra
import Network.Wai.Test

spec :: Spec
spec = do
  describe "GET /" $
    it "returns the home page" $
      withApp $ do
        response <- request (setPath defaultRequest "/")
        assertStatus 200 response
        assertBodyContains "Shorten" response
        assertBodyContains "ID" response
        assertBodyContains "URL (Long)" response
        assertBodyContains "URL (Short)" response
        assertBodyContains "Clicks" response
        assertBodyContains "..." response
  describe "POST /" $
    it "creates a URL" $
      withApp $ do
        let req = setPath defaultRequest "/?url=localhost"
        response <- request (req { requestMethod = "POST" })
        assertStatus 302 response
  where
    withApp f = do
      removeFile "shortener-test.db"
      app <- todoApp "shortener-test.db"
      flip runSession app f