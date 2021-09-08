{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Init
  ( runApp,
  )
where

import Api (app)
import Api.Gifs.Models (Channel (..))
import Config
  ( Config (..),
    Environment (..),
    StremConfig (..),
    setLogger,
  )
import Control.Concurrent.STM as STM
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Logger (defaultLogEnv)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import qualified Network.WebSockets as Ws
import Obs (connectToObsWs)
import System.Directory (getHomeDirectory)
import System.Environment.Extra (lookupSetting)
import System.FilePath ((</>))
import Prelude

runApp :: IO ()
runApp =
  Ws.runClient "localhost" 4444 "/" $ \client -> do
    connectToObsWs client

    runApp' =<< acquireConfig client

    -- TODO: use bracket instead ? Or intercept kill signal ?
    Ws.sendClose client (Text.pack "Bye!")
  where
    runApp' config = run (configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Wai.Application
initialize cfg = do
  pure . setLogger (configEnv cfg) . corsified . app $ cfg

-- | Allocates resources for 'Config'
acquireConfig :: Ws.Connection -> IO Config
acquireConfig client = do
  port <- lookupSetting "PORT" 9000
  env <- lookupSetting "ENV" Development
  logEnv <- defaultLogEnv
  channel <- STM.atomically $ STM.newTVar . Channel IM.empty =<< STM.newTVar 0
  stremConfig <- Yaml.decodeFileEither @StremConfig =<< (</> ".strem.yml") <$> getHomeDirectory
  pure
    Config
      { configPort = port,
        configEnv = env,
        configLogEnv = logEnv,
        configOverlayChannel = channel,
        configObsWsClient = client,
        configScenes = either (const Map.empty) stremScenes stremConfig
      }

corsified :: Wai.Middleware
corsified = cors (const $ Just corsResourcePolicy)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["Authorization", "Content-Type"],
      corsExposedHeaders = Nothing,
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }
