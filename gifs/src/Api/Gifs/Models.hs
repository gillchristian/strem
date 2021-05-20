{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api.Gifs.Models
  ( Message (..),
    Channel (..),
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Json
import Data.Either (Either (..))
import qualified Data.IntMap as IM
import GHC.Generics
import General.Util (dropLabelPrefix)
import qualified Network.WebSockets.Connection as Ws
import Text.Casing (camel)
import Prelude

data Message
  = Message
  | Gif {gif :: String}
  | Audio {audio :: String}
  deriving stock (Generic, Show)
  deriving (Json.ToJSON, Json.FromJSON)

data Channel = Channel
  { chanListeners :: IM.IntMap Ws.Connection,
    chanIdCounter :: STM.TVar Int
  }
