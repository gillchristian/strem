{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Gifs
  ( GifsAPI,
    gifsApi,
    gifsServer,
  )
where

import Api.Gifs.Models
import Config (AppT (..), Config (..))
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import qualified Data.IntMap as IM
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified General.Websockets as Ws
import qualified Network.WebSockets as Ws
import Obs (mkObsWsMsg)
import Servant ((:<|>) (..), (:>))
import qualified Servant
import Servant.API.WebSocket (WebSocketPending)
import Prelude hiding (filter)

type GifsAPI = GifsWsRoute :<|> MessageRounte

type GifsWsRoute = "ws" :> WebSocketPending

type MessageRounte =
  "gif"
    :> Servant.ReqBody '[Servant.JSON] Message
    :> Servant.Post '[Servant.JSON] ()

gifsServer :: MonadIO m => Servant.ServerT GifsAPI (AppT m)
gifsServer = gifsWsHandler :<|> messageHandler

gifsApi :: Servant.Proxy GifsAPI
gifsApi = Servant.Proxy

messageHandler :: MonadIO m => Message -> AppT m ()
messageHandler (Scene name) = do
  liftIO $ putStrLn $ "Attempting to switch scene to: " <> name
  client <- asks configObsWsClient
  liftIO $ Ws.sendTextData client $ mkObsWsMsg "SetCurrentScene" ["scene-name" .= name]
  pure ()
messageHandler msg = do
  channel <- asks configOverlayChannel
  liftIO $ broadcast channel $ decodeUtf8 $ toStrict $ Json.encode msg
  pure ()

-- Client connection
gifsWsHandler :: MonadIO m => Ws.PendingConnection -> AppT m ()
gifsWsHandler pendingC = do
  c <- liftIO $ Ws.acceptRequest pendingC
  channel <- asks configOverlayChannel

  listenerId <- liftIO $
    STM.atomically $ do
      id <- mkListenerId channel
      insertListener channel id c
      pure id

  liftIO $
    Exception.finally
      (pingThreadWithBlock c)
      (cleanupListener c listenerId channel)

pingThreadWithBlock :: Ws.Connection -> IO ()
pingThreadWithBlock c = Ws.pingThread c (Ws.receiveData c :: IO Text)

cleanupListener :: Ws.Connection -> Int -> STM.TVar Channel -> IO ()
cleanupListener c listenerId channel = do
  STM.atomically $ disconnectListener channel listenerId
  Ws.sendClose c ("Bye" :: Text)

broadcast :: STM.TVar Channel -> Text -> IO ()
broadcast chan msg = do
  chan <- STM.readTVarIO chan
  Monad.forM_ (chanListeners chan) $ \c ->
    Exception.catch (Ws.sendTextData c msg) Ws.ignoreConnectionException

mkListenerId :: STM.TVar Channel -> STM Int
mkListenerId channel = do
  chan <- STM.readTVar channel
  i <- STM.readTVar $ chanIdCounter chan
  let next = i + 1
  STM.writeTVar (chanIdCounter chan) next
  pure next

insertListener :: STM.TVar Channel -> Int -> Ws.Connection -> STM ()
insertListener channel listenerId c =
  STM.modifyTVar channel insertConnection
  where
    insertConnection :: Channel -> Channel
    insertConnection chan = chan {chanListeners = IM.insert listenerId c $ chanListeners chan}

disconnectListener :: STM.TVar Channel -> Int -> STM (Maybe Ws.Connection)
disconnectListener channel listenerId = do
  mc <- IM.lookup listenerId . chanListeners <$> STM.readTVar channel
  STM.modifyTVar channel $ removeListener listenerId
  pure mc
  where
    removeListener :: Int -> Channel -> Channel
    removeListener listenerId chan =
      chan {chanListeners = IM.delete listenerId $ chanListeners chan}
