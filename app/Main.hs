{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import Fetch ( fetchFromDb, webScrape )
import Web.Scotty ( get, json, param, scotty, text, status, ActionM )
import Database.PostgreSQL.Simple
    ( connect,
      defaultConnectInfo,
      ConnectInfo(connectPassword, connectHost, connectDatabase,
                  connectUser),
      Connection )

import Configuration.Dotenv
    ( loadFile,
      Config(Config, configExamplePath, configPath, configOverride,
             configVerbose, allowDuplicates) )
import System.Environment (getEnv)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Applicative ( Alternative((<|>)) )
import Network.HTTP.Types.Status (status200, status400)
import Result ( Result )

routes :: Connection -> IO ()
routes conn = scotty 3000 $ do
  get "/average-score/:name" $ do
    name <- param "name"
    result <- liftIO $ runMaybeT $ fetchFromDb conn name <|> webScrape conn name
    maybe notFound handleResponse result

handleResponse :: Result -> ActionM ()
handleResponse result = status status200 >> json result

notFound :: ActionM ()
notFound = status status400 >> text "Professor name not found"

main :: IO ()
main = do
  putStrLn "Starting server..."

  let envConfig = Config {
      configPath = ["./credentials.env"]
    , configOverride = True
    , configVerbose = True
    , allowDuplicates = False
    , configExamplePath = []
  }

  loadFile envConfig

  postgresHost <- getEnv "POSTGRES_HOST"
  postgresDatabase <- getEnv "POSTGRES_DB"
  postgresUser <- getEnv "POSTGRES_USER"
  postgresPassword <- getEnv "POSTGRES_PASSWORD"

  let localPG = defaultConnectInfo {
      connectHost = postgresHost
    , connectDatabase = postgresDatabase
    , connectUser = postgresUser
    , connectPassword = postgresPassword
  }

  conn <- connect localPG
  routes conn