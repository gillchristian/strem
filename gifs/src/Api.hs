{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app,
  )
where

import Api.Gifs (GifsAPI, gifsApi, gifsServer)
import Config (AppT (..), Config (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Servant
  ( (:<|>) (..),
    (:>),
  )
import qualified Servant
import Servant.Server
import Prelude

gifsApp :: Config -> Application
gifsApp cfg = Servant.serve appApi (appToServer cfg)

appToServer :: Config -> Servant.Server AppAPI
appToServer cfg = hoistServer appApi (convertApp cfg) appServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

-- TODO: change paths
type AppAPI = HealthzRoute :<|> "gifs" :> GifsAPI

healthzHandler :: MonadIO m => AppT m Text
healthzHandler = pure "200 Ok"

type HealthzRoute =
  "healthz"
    :> Servant.Get '[Servant.PlainText] Text

appServer :: MonadIO m => Servant.ServerT AppAPI (AppT m)
appServer = healthzHandler :<|> gifsServer

appApi :: Servant.Proxy AppAPI
appApi = Servant.Proxy

app :: Config -> Application
app cfg = serve appApi $ appToServer cfg
