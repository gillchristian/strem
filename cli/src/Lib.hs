{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Lens hiding ((.=))
import Control.Monad (unless, void, when)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, isJust)
import Data.Traversable (forM)
import GHC.Generics (Generic)
import Network.Wreq hiding (Options)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

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

data CompleteNextOptions = CompleteNextOptions
  { doneBinId :: String,
    doneNextYes :: Bool
  }
  deriving (Eq, Show)

completeOpts :: Opt.ParserInfo Command
completeOpts =
  CompletNext <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Complete the next future event"
    opts =
      CompleteNextOptions
        <$> Opt.strOption
          ( Opt.long "bin-id"
              <> Opt.short 'b'
              <> Opt.metavar "BIN_ID"
              <> Opt.help "JsonBIN Bin ID"
              <> Opt.showDefault
          )
        <*> Opt.switch
          ( Opt.long "yes"
              <> Opt.short 'y'
              <> Opt.help "Do not prompt confirmation before saving"
              <> Opt.showDefault
          )

data AddFutureOptions = AddFutureOptions
  { addBinId :: String,
    addNextYes :: Bool,
    addCopyLastFuture :: Bool
  }
  deriving (Eq, Show)

addFutureOpts :: Opt.ParserInfo Command
addFutureOpts =
  AddFuture <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Add a new future event"
    opts =
      AddFutureOptions
        <$> Opt.strOption
          ( Opt.long "bin-id"
              <> Opt.short 'b'
              <> Opt.metavar "BIN_ID"
              <> Opt.help "JsonBIN Bin ID"
              <> Opt.showDefault
          )
        <*> Opt.switch
          ( Opt.long "yes"
              <> Opt.short 'y'
              <> Opt.help "Do not prompt confirmation before saving"
              <> Opt.showDefault
          )
        <*> Opt.switch
          ( Opt.long "copy-last"
              <> Opt.short 'c'
              <> Opt.help "Copy topic and description from the last event in future list"
              <> Opt.showDefault
          )

data Command
  = CompletNext CompleteNextOptions
  | AddFuture AddFutureOptions
  deriving (Eq, Show)

newtype Options = Options {uncommand :: Command}
  deriving (Eq, Show)

opts :: Opt.ParserInfo Options
opts =
  Opt.info
    (programm <**> Opt.helper)
    (Opt.fullDesc <> Opt.header "Handle my stream schedule")
  where
    programm = Options <$> Opt.hsubparser (complete <> new)
    complete = Opt.command "done" completeOpts
    new = Opt.command "new" addFutureOpts

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

prompt :: String -> IO String
prompt msg = putStr (msg <> ": ") *> hFlush stdout *> getLine

confirm :: String -> IO ()
confirm msg =
  void $ prompt (msg <> " (press Enter to confirm or Ctrl + C to cancel)")

promptOptional :: Maybe String -> String -> IO (Maybe String)
promptOptional Nothing msg =
  nonEmpty <$> prompt (msg <> " (press Enter to ignore)")
promptOptional (Just prev) msg =
  Just . fromMaybe prev . nonEmpty <$> prompt (msg <> " (current: '" <> prev <> "', press Enter to use it)")

promptTopic :: Maybe Topic -> IO Topic
promptTopic Nothing = do
  label <- prompt "What's the stream topic?"
  url <- promptOptional Nothing "Topic URL?"
  pure $ Topic url label
promptTopic (Just (Topic mbUrl label)) = do
  mbNewLabel <- promptOptional (Just label) "New topic?"
  mbNewUrl <- promptOptional mbUrl "Topic URL?"
  pure $ Topic mbNewUrl $ fromMaybe label mbNewLabel

promptMbTopic :: IO (Maybe Topic)
promptMbTopic = do
  mbLabel <- promptOptional Nothing "What's the stream topic?"
  forM mbLabel $ \label -> do
    url <- promptOptional Nothing "Topic URL? (press Enter to ignore)"
    pure $ Topic url label

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM f ma = do
  a <- ma
  if f a then pure a else untilM f ma

futureToPast :: FutureEvent -> IO PastEvent
futureToPast e = do
  topic <- promptTopic $ future_topic e
  -- TODO: add now
  end_date <- untilM (not . null) $ prompt $ "When did the stream end? (started at: " <> future_start_date e <> ")"
  description <- promptOptional (future_description e) "Description?"
  pure $ PastEvent (future_start_date e) end_date topic description Nothing

promptFutureEvent :: IO FutureEvent
promptFutureEvent = do
  mbTopic <- promptMbTopic
  description <- promptOptional Nothing "Description?"
  start_date <- untilM (not . null) $ prompt "When will the stream start?"
  pure $ FutureEvent start_date mbTopic description

updateBin :: String -> String -> Events -> IO ()
updateBin secret id events = do
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
        <> maybe "<no status>" BS.unpack (r ^? responseStatus . statusMessage)
        <> ")"

fetchEvents :: String -> IO (Maybe Events)
fetchEvents id = do
  putStrLn "Fetching events ..."
  r <- asJSON =<< getWith reqConfg url
  pure $ r ^? responseBody
  where
    -- curl https://api.jsonbin.io/b/<id> -H "Accept: application/json" \
    url = "https://api.jsonbin.io/b/" <> id <> "/latest"
    reqConfg = defaults & header "Content-Type" .~ ["application/json"]

printPastEvent :: PastEvent -> IO ()
printPastEvent e = do
  putStrLn $ "Started at:  " <> past_start_date e
  putStrLn $ "Ended at:    " <> past_end_date e
  putStrLn $ "Topic:       " <> topic_label (past_topic e)
  traverse_ putStrLn $ ("URL:         " <>) <$> topic_url (past_topic e)
  traverse_ putStrLn $ ("Description: " <>) <$> past_description e

printFutureEvent :: FutureEvent -> IO ()
printFutureEvent e = do
  putStrLn $ "Starts at:   " <> future_start_date e
  traverse_ putStrLn $ ("Topic:       " <>) . topic_label <$> future_topic e
  traverse_ putStrLn $ ("URL:         " <>) <$> (topic_url =<< future_topic e)
  traverse_ putStrLn $ ("Description: " <>) <$> future_description e

dropNextAddPast :: Events -> PastEvent -> Events
dropNextAddPast (Events future past) e = Events (tail future) $ e : past

completeNext :: CompleteNextOptions -> String -> IO ()
completeNext (CompleteNextOptions id yes) secret = do
  mbEvents <- fetchEvents id
  case mbEvents of
    Nothing -> putStrLn "Could not fetch events"
    Just events ->
      case safeHead $ future events of
        Nothing -> putStrLn "No future events found"
        Just next -> do
          toSave <- futureToPast next
          let future = dropNextAddPast events toSave
          when yes $ updateBin secret id future
          unless yes $ do
            putStrLn "\n--- Here's what will be saved ---"
            printPastEvent toSave
            confirm "\nSave?"
            updateBin secret id future

appendFutureEvent :: Events -> FutureEvent -> Events
appendFutureEvent (Events future past) e = Events (future <> [e]) past

addFuture :: AddFutureOptions -> String -> IO ()
addFuture (AddFutureOptions id yes copyLastFuture) secret = do
  mbEvents <- fetchEvents id
  case mbEvents of
    Nothing -> putStrLn "Could not fetch events"
    Just events -> do
      future <- case (copyLastFuture, safeHead $ reverse $ future events) of
        (True, Just e) -> do
          putStrLn "Copying event from:"
          printFutureEvent e *> putStrLn ""
          start_date <- untilM (not . null) $ prompt "When will the stream start?"
          pure $ FutureEvent start_date (future_topic e) (future_description e)
        (True, Nothing) -> do
          putStrLn "No future events to copy from"
          promptFutureEvent
        (False, _) -> promptFutureEvent

      let updatedEvents = appendFutureEvent events future
      when yes $ updateBin secret id updatedEvents
      unless yes $ do
        putStrLn "\n--- Here's what will be saved ---"
        printFutureEvent future
        confirm "\nSave?"
        updateBin secret id updatedEvents

readSecret :: IO String
readSecret = do
  secretPath <- (</> ".stremsecret") <$> getHomeDirectory
  filter (not . isSpace) <$> readFile secretPath

run :: IO ()
run = do
  cmd <- uncommand <$> Opt.execParser opts
  case cmd of
    CompletNext opts -> completeNext opts =<< readSecret
    AddFuture opts -> addFuture opts =<< readSecret
