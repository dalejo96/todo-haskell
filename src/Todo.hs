{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Todo (todo, todoApp) where

-- base
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)

-- blaze-html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- blaze-markup
import Text.Blaze.Internal (attribute)

-- http-types
import Network.HTTP.Types (status404)

-- persistent
import qualified Database.Persist.Sql as P

-- persistent-sqlite
import Database.Persist.Sqlite (runMigration, runSqlite)

-- persitent-template
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

-- random
import System.Random (randomRIO)

-- scotty
import Web.Scotty

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- time
import Data.Time (UTCTime, getCurrentTime)

-- wai
import Network.Wai (Application)

share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
Url
  long Text
  short Text
  clicks Int
  createdAt UTCTime
  updatedAt UTCTime Maybe
  UniqueUrlShort short
  deriving Show
|]

todo :: IO ()
todo = do
  runSqlite "todo.db" $ runMigration migrate
  scotty 3000 (todoScottyApp "todo.db")

todoApp :: Text -> IO Application
todoApp connectionString = do
  runSqlite connectionString $ runMigration migrate
  scottyApp (todoScottyApp connectionString)

todoScottyApp :: Text -> ScottyM ()
todoScottyApp connectionString = do
  get "/" $ do
    urls <- runDB $ P.selectList [] [P.Asc UrlId]
    html $ renderHtml $
      H.html $ do
        H.head $ do
          H.title "To-Do"
          bootstrapCss
        H.body H.! A.class_ "container" $ do
          H.header $
            H.h1 "To-Do List"
          H.form H.! A.action "/" H.! A.method "post" $ do
            H.div H.! A.class_ "row mb-3" $ do
              H.label H.! A.class_ "visually-hidden" H.! A.for "url" $
                H.text "URL"
              H.div $
                H.input
                  H.! A.class_ "form-control"
                  H.! A.id "url"
                  H.! A.name "url"
                  H.! A.required "required"
                  H.! A.type_ "text"
            H.div $
              H.button H.! A.class_ "btn btn-primary" H.! A.type_ "submit" $
                H.text "To-Do"
          H.div H.! A.class_ "table-responsive" $
            H.table H.! A.class_ "table table-bordered table-striped" $ do
              H.thead $
                H.tr $ do
                  H.th (H.text "ID")
                  H.th (H.text "URL (Short)")
                  H.th (H.text "URL (Long)")
                  H.th (H.text "Clicks")
              H.tbody $ do
                when (null urls) $
                  H.tr $
                    H.td H.! A.colspan "4" $
                      H.text "..."
                for_ urls $ \(P.Entity urlId url) ->
                  H.tr $ do
                    H.td (H.toHtml (P.fromSqlKey urlId))
                    H.td $
                      H.a
                        H.! A.href (H.textValue (urlShort url))
                        H.! A.target "todo" $
                          H.text (urlShort url)
                    H.td (H.text (urlLong url))
                    H.td (H.text (T.pack (show (urlClicks url))))
  post "/" $ do
    long <- param "url"
    short <- generateShortUrl
    now <- liftIO getCurrentTime
    runDB $ P.insert_ Url
      { urlShort = short
      , urlLong = long
      , urlClicks = 0
      , urlCreatedAt = now
      , urlUpdatedAt = Nothing
      }
    redirect "/"
  get "/:short" $ do
    short <- param "short"
    mUrl <- runDB $ P.getBy (UniqueUrlShort short)
    case mUrl of
      Just (P.Entity urlId Url { urlLong }) -> do
        now <- liftIO getCurrentTime
        runDB $ P.update urlId
          [ UrlClicks P.+=. 1
          , UrlUpdatedAt P.=. Just now
          ]
        redirect (LT.fromStrict urlLong)
      Nothing ->
        raiseStatus status404 ""
  where
    runDB :: P.SqlPersistM a -> ActionM a
    runDB =
      liftIO . runSqlite connectionString

generateShortUrl :: MonadIO m => m Text
generateShortUrl =
  T.pack <$> replicateM 6 (randomRIO ('a', 'z'))

bootstrapCss :: H.Html
bootstrapCss =
  H.link
    H.! A.href href
    H.! A.rel "stylesheet"
    H.! attribute "crossorigin" " crossorigin=\"" "anonymous"
    H.! attribute "integrity" " integrity=\"" integrity
  where
    href =
      "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/css/bootstrap.min.css"
    integrity =
      "sha384-+0n0xVW2eSR5OomGNYDnhzAbDsOXxcvSN1TPprVMTNDbiYZCxYbOOl7+AMvyTG2x"