{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Lens hiding ((.=))
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import Network.Wreq hiding (Options)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt

data Topic = Topic
  { topic_url :: Maybe String,
    topic_label :: String
  }
  deriving (Show)

data FutureEvent = FutureEvent
  { future_start_date :: String,
    future_topic :: Maybe Topic,
    future_description :: Maybe String
  }
  deriving (Show)

instance Json.ToJSON FutureEvent where
  toJSON event =
    Json.object
      [ "start_date" .= future_start_date event,
        "topic_url" .= fmap topic_url (future_topic event),
        "topic_label" .= fmap topic_label (future_topic event),
        "description" .= future_description event
      ]

instance Json.FromJSON FutureEvent where
  parseJSON =
    Json.withObject "FutureEvent" $ \obj -> do
      start_date <- obj .: "start_date"
      topic_label <- obj .:? "topic_label"
      topic_url <- obj .:? "topic_url"
      description <- obj .:? "description"

      pure $ FutureEvent start_date (Topic topic_url <$> topic_label) description

data PastEvent = PastEvent
  { past_start_date :: String,
    past_end_date :: String,
    past_topic :: Topic,
    past_description :: Maybe String,
    past_vod :: Maybe String
  }
  deriving (Show)

instance Json.FromJSON PastEvent where
  parseJSON =
    Json.withObject "FutureEvent" $ \obj -> do
      start_date <- obj .: "start_date"
      end_date <- obj .: "end_date"
      topic_label <- obj .: "topic_label"
      topic_url <- obj .:? "topic_url"
      description <- obj .:? "description"
      vod <- obj .:? "vod"

      pure $ PastEvent start_date end_date (Topic topic_url topic_label) description vod

instance Json.ToJSON PastEvent where
  toJSON event =
    Json.object
      [ "start_date" .= past_start_date event,
        "end_date" .= past_end_date event,
        "topic_url" .= topic_url (past_topic event),
        "topic_label" .= topic_label (past_topic event),
        "description" .= past_description event,
        "vod" .= past_vod event
      ]

data Events = Events
  { future :: [FutureEvent],
    past :: [PastEvent]
  }
  deriving (Show, Generic, Json.FromJSON, Json.ToJSON)

-- --- CLI ---

newtype CompletNextOptions = CompletNextOptions {binId :: String}
  deriving (Eq, Show)

completeOpts :: Opt.ParserInfo Command
completeOpts =
  CompletNext <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Complete the next future event"
    opts =
      CompletNextOptions
        <$> Opt.strOption
          ( Opt.long "bin-id"
              <> Opt.short 'b'
              <> Opt.metavar "BIN_ID"
              <> Opt.help "JsonBIN Bin ID"
              <> Opt.showDefault
          )

newtype Command
  = CompletNext CompletNextOptions
  deriving (Eq, Show)

newtype Options = Options {uncommand :: Command}
  deriving (Eq, Show)

opts :: Opt.ParserInfo Options
opts =
  Opt.info
    (programm <**> Opt.helper)
    (Opt.fullDesc <> Opt.header "Handle my stream schedule")
  where
    programm = Options <$> Opt.hsubparser complete
    complete = Opt.command "done" completeOpts

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

prompt :: String -> IO String
prompt msg = putStrLn (msg <> ": ") *> getLine

promptTopic :: Maybe Topic -> IO Topic
promptTopic Nothing = do
  label <- prompt "What's the stream topic?"
  url <- nonEmpty <$> prompt "Topic URL? (press Enter to ignore)"
  pure $ Topic url label
promptTopic (Just (Topic mbUrl label)) = do
  mbNewLabel <- nonEmpty <$> prompt ("New topic? (current: '" <> label <> "', press Enter to ignore)")
  mbNewUrl <- case mbUrl of
    Nothing -> nonEmpty <$> prompt "Topic URL? (press Enter to ignore)"
    Just url -> do
      mbNewUrl <- nonEmpty <$> prompt ("New Topic URL? (current: '" <> url <> "', press Enter to ignore)")
      pure $ if isJust mbNewUrl then mbNewUrl else Just url
  pure $ Topic mbNewUrl $ fromMaybe label mbNewLabel

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM f ma = do
  a <- ma
  if f a then pure a else untilM f ma

futureToPast :: FutureEvent -> IO PastEvent
futureToPast e = do
  topic <- promptTopic $ future_topic e
  -- TODO: add now
  end_date <- untilM (not . null) $ prompt $ "When did the stream end? (started at: " <> future_start_date e <> ")"
  description <- nonEmpty <$> prompt "Description (press Enter to ignore)"
  pure $ PastEvent (future_start_date e) end_date topic description Nothing

updateBin :: String -> CompletNextOptions -> Events -> IO ()
updateBin secret (CompletNextOptions id) events = do
  putStrLn "Saving events ..."
  r <- putWith reqConfg url $ Json.toJSON events
  putStrLn $ successMsg r
  where
    -- curl https://api.jsonbin.io/b/<id> \
    -- -X PUT \
    -- -H "secret-key: <secret-key>" \
    -- -H "Accept: application/json" \
    -- -H "versioning: false" \
    -- -d '<bin json>'
    reqConfg =
      defaults
        & header "Content-Type" .~ ["application/json"]
        & header "secret-key" .~ [BS.pack secret]
        & header "versioning" .~ ["false"]
    url = "https://api.jsonbin.io/b/" <> id
    successMsg r =
      "✓ Stream events updated ("
        <> maybe "<no status>" show (r ^? responseStatus)
        <> ")"

foo :: PastEvent -> Events -> Events
foo e (Events future past) = Events (tail future) $ e : past

completeNext :: String -> CompletNextOptions -> IO ()
completeNext secret opts = do
  mbEvents :: Maybe Events <- Json.decodeFileStrict "events.json"
  case mbEvents of
    Nothing -> putStrLn "Could not read events"
    Just events ->
      case safeHead $ future events of
        Nothing -> putStrLn "No future events found"
        Just next -> do
          future <- futureToPast next
          updateBin secret opts $ foo future events

run :: IO ()
run = do
  secret <- filter (not . isSpace) <$> readFile "secret.txt"
  cmd <- uncommand <$> Opt.execParser opts
  case cmd of
    CompletNext opts -> completeNext secret opts
