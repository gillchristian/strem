{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Concurrent (forkIO)
import Control.Concurrent.STM as STM
import Control.Monad (forever, void)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import qualified Data.ByteString.Lazy.Char8 as LazyBSIO
import qualified Data.IntMap as IM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Logger (defaultLogEnv)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import qualified Network.WebSockets as Ws
import Safe (readMay)
import System.Environment (lookupEnv)
import qualified System.Exit as Sys
import Prelude

msgToText :: Ws.DataMessage -> Either String LazyBS.ByteString
msgToText (Ws.Text msg _) = Right msg
-- msgToText (Ws.Text msg Nothing) = Left $ "Could not UTF-8 decode msg (" <> show msg <> ")"
msgToText (Ws.Binary _) = Left "Got binary message"

data GetAuthRequiredRes
  = GetAuthSuccess {resSalt :: Text, resChallenge :: Text}
  | GetAuthFail String
  | GetAuthOtherMsg Bool String
  deriving (Eq, Show)

instance Json.FromJSON GetAuthRequiredRes where
  parseJSON =
    Json.withObject "GetAuthRequiredRes" $ \obj -> do
      status :: String <- obj .: "status"
      id <- obj .: "message-id"

      case (id, status) of
        ("GetAuthRequired", "ok") ->
          GetAuthSuccess <$> obj .: "salt" <*> obj .: "challenge"
        ("GetAuthRequired", _) ->
          GetAuthFail <$> obj .: "error"
        (other, _) ->
          pure $ GetAuthOtherMsg (status == "ok") other

data AuthenticateRes
  = AuthenticateSuccess
  | AuthenticateFail String
  | AuthenticateOtherMsg Bool String
  deriving (Eq, Show)

instance Json.FromJSON AuthenticateRes where
  parseJSON =
    Json.withObject "AuthenticateRes" $ \obj -> do
      status :: String <- obj .: "status"
      id <- obj .: "message-id"

      case (id, status) of
        ("Authenticate", "ok") ->
          pure AuthenticateSuccess
        ("Authenticate", _) ->
          AuthenticateFail <$> obj .: "error"
        (other, _) ->
          pure $ AuthenticateOtherMsg (status == "ok") other

waitForGetAuthRes :: Ws.Connection -> IO (Either String (Text, Text))
waitForGetAuthRes client = do
  res <- msgToText <$> Ws.receiveDataMessage client

  case res >>= Json.eitherDecode of
    Left err -> pure $ Left err
    Right (GetAuthSuccess salt challenge) -> pure $ Right (salt, challenge)
    Right (GetAuthFail err) -> pure $ Left err
    Right (GetAuthOtherMsg _ _) -> waitForGetAuthRes client

waitForAuthenticateRes :: Ws.Connection -> IO (Either String ())
waitForAuthenticateRes client = do
  res <- msgToText <$> Ws.receiveDataMessage client

  case res >>= Json.eitherDecode of
    Left err -> pure $ Left err
    Right AuthenticateSuccess -> pure $ Right ()
    Right (AuthenticateFail err) -> pure $ Left err
    Right (AuthenticateOtherMsg _ _) -> waitForAuthenticateRes client

makeSecret :: (String, Text, Text) -> Text
makeSecret (pass, salt, challenge) =
  decodeUtf8 $ encode $ encode secret <> encodeUtf8 challenge
  where
    encode :: BS.ByteString -> BS.ByteString
    encode = encodeUtf8 . encodeBase64 . SHA256.hash
    secret = encodeUtf8 (Text.pack pass <> salt)

mkObsWsMsg :: String -> [Json.Pair] -> LazyBS.ByteString
mkObsWsMsg type' fields =
  Json.encode $
    Json.object (["request-type" .= type', "message-id" .= type'] <> fields)

runApp :: IO ()
runApp =
  Ws.runClient "localhost" 4444 "/" $ \client -> do
    let getAuthRequired = mkObsWsMsg "GetAuthRequired" []

    Ws.sendTextData client getAuthRequired

    res <- waitForGetAuthRes client

    (salt, challenge) <- case res of
      Right values -> pure values
      Left err -> Sys.die err

    -- TODO: proper logs
    putStrLn "Got the challenge"

    -- TODO: encoding
    obsWsPass <- lookupSetting "OBS_WS_PASS" "default-password-asdf"
    let secret = makeSecret (obsWsPass, salt, challenge)
        authenticate = mkObsWsMsg "Authenticate" ["auth" .= secret]

    Ws.sendTextData client authenticate

    res <- waitForAuthenticateRes client

    case res of
      Right _ -> pure ()
      Left err -> Sys.die err

    -- TODO: proper logs
    putStrLn "Auth ok"

    -- TODO: use withAsync & withCatch instead of forkIO ?
    void . forkIO . forever $ do
      msg <- msgToText <$> Ws.receiveDataMessage client
      case msg of
        Left err -> putStrLn $ "Error: " <> err
        Right msg -> LazyBSIO.putStrLn msg

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
  pure
    Config
      { configPort = port,
        configEnv = env,
        configLogEnv = logEnv,
        configOverlayChannel = channel,
        configObsWsClient = client
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
