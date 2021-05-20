{-# LANGUAGE OverloadedStrings #-}

module Init
  ( runApp,
  )
where

import Api (app)
import Api.Gifs.Models (Channel (..))
import Config
  ( Config (..),
    Environment (..),
    setLogger,
  )
import Control.Concurrent (killThread)
import Control.Concurrent.STM as STM
import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict as HM hiding (null)
import qualified Data.IntMap as IM
import Data.Text.Encoding (encodeUtf8)
import Logger (defaultLogEnv)
import Network.HTTP.Types.Status (status302)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Safe (readMay)
import System.Environment (lookupEnv)
import Prelude

runApp :: IO ()
runApp =
  runApp' =<< acquireConfig
  where
    runApp' config = run (configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Wai.Application
initialize cfg = do
  pure . setLogger (configEnv cfg) . corsified . app $ cfg

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
  port <- lookupSetting "PORT" 9000
  env <- lookupSetting "ENV" Development
  logEnv <- defaultLogEnv
  clientUrl <- lookupSetting "CLIENT_URL" "http://localhost:9000"
  channel <- STM.atomically $ STM.newTVar . Channel IM.empty =<< STM.newTVar 0
  pure
    Config
      { configPort = port,
        configEnv = env,
        configLogEnv = logEnv,
        configClientUrl = clientUrl,
        configChannel = channel
      }

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      pure def
    Just str ->
      maybe (error $ failMsg str) pure (readMay str)
  where
    failMsg str = mconcat ["Failed to read [[", str, "]] for environment variable ", env]

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
