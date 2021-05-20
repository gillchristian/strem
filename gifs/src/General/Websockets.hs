{-# LANGUAGE OverloadedStrings #-}

module General.Websockets
  ( rejectNotFound,
    rejectUnauthorized,
    pingThread,
    ignoreConnectionException,
  )
where

import qualified Control.Monad as Monad
import qualified Network.WebSockets as Ws

rejectNotFound :: Ws.PendingConnection -> IO ()
rejectNotFound pendingC =
  Ws.rejectRequestWith pendingC $
    Ws.defaultRejectRequest {Ws.rejectCode = 404, Ws.rejectMessage = "Not found"}

rejectUnauthorized :: Ws.PendingConnection -> IO ()
rejectUnauthorized pendingC =
  Ws.rejectRequestWith pendingC $
    Ws.defaultRejectRequest {Ws.rejectCode = 401, Ws.rejectMessage = "Unauthorized"}

pingThread :: Ws.Connection -> IO a -> IO ()
pingThread c action =
  Ws.withPingThread c 30 mempty $ Monad.void action

ignoreConnectionException :: Ws.ConnectionException -> IO ()
ignoreConnectionException _ = mempty
