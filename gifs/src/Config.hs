{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config
  ( AppT (..),
    App,
    Config (..),
    Environment (..),
    StremConfig (..),
    setLogger,
  )
where

import Api.Gifs.Models
import Control.Concurrent.STM as STM
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger (..))
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    asks,
  )
import qualified Data.Aeson as Json
import Data.Map (Map)
import GHC.Generics (Generic)
import General.Util (dropLabelPrefix)
import Logger
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Network.WebSockets as Ws
import Servant.Server (ServerError)
import Prelude

newtype AppT m a = AppT {runApp :: ReaderT Config (ExceptT ServerError m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

instance MonadIO m => Katip (AppT m) where
  getLogEnv = asks configLogEnv

  localLogEnv = error "not implemented"

instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = adapt logMsg

newtype StremConfig = StremConfig {stremScenes :: Map String String}
  deriving stock (Show, Generic)

instance Json.ToJSON StremConfig where
  toJSON = Json.genericToJSON $ dropLabelPrefix "strem"

instance Json.FromJSON StremConfig where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "strem"

data Config = Config
  { configPort :: Port,
    configEnv :: Environment,
    configLogEnv :: LogEnv,
    configOverlayChannel :: STM.TVar Channel,
    configObsWsClient :: Ws.Connection,
    configScenes :: Map String String
  }

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout
