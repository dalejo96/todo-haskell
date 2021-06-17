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
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)

-- blaze-html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- blaze-markup
import Text.Blaze.Internal (attribute)

-- persistent
import qualified Database.Persist.Sql as P

-- persistent-sqlite
import Database.Persist.Sqlite (runMigration, runSqlite)

-- persitent-template
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

-- scotty
import Web.Scotty

-- text
import Data.Text (Text)

-- time
import Data.Time (UTCTime, getCurrentTime)

-- wai
import Network.Wai (Application)

share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
Todos
  title Text
  description Text
  clicks Int
  createdAt UTCTime
  updatedAt UTCTime Maybe
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
    items <- runDB $ P.selectList [] [P.Asc TodosId]
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
              H.label H.! A.for "title" $
                H.text "Title"
              H.div $
                H.input
                  H.! A.class_ "form-control"
                  H.! A.id "title"
                  H.! A.name "title"
                  H.! A.required "required"
                  H.! A.placeholder "Insert your title here"
                  H.! A.type_ "text"
            H.div H.! A.class_ "row mb-3" $ do
              H.label H.! A.for "description" $
                H.text "Description"
              H.div $
                H.input
                  H.! A.class_ "form-control"
                  H.! A.id "description"
                  H.! A.name "description"
                  H.! A.placeholder "Insert your description here"
                  H.! A.required "required"
                  H.! A.type_ "text"
            H.div $
              H.button H.! A.class_ "btn btn-primary" H.! A.type_ "submit" $
                H.text "Add To-Do item"
          H.div H.! A.class_ "table-responsive" $
            H.table H.! A.class_ "table table-bordered table-striped" $ do
              H.thead $
                H.tr $ do
                  H.th (H.text "ID")
                  H.th (H.text "Title")
                  H.th (H.text "Description")
              H.tbody $ do
                when (null items) $
                  H.tr $
                    H.td H.! A.colspan "4" $
                      H.text "..."
                for_ items $ \(P.Entity itemId item) ->
                  H.tr $ do
                    H.td (H.toHtml (P.fromSqlKey itemId))
                    H.td (H.text (todosTitle item))
                    H.td (H.text (todosDescription item))
  post "/" $ do
    title <- param "title"
    description <- param "description"
    now <- liftIO getCurrentTime
    runDB $ P.insert_ Todos
      { todosTitle = title
      , todosDescription = description
      , todosClicks = 0
      , todosCreatedAt = now
      , todosUpdatedAt = Nothing
      }
    redirect "/"
   where
    runDB :: P.SqlPersistM a -> ActionM a
    runDB =
      liftIO . runSqlite connectionString

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