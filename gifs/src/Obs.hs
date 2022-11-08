{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Obs (connectToObsWs, mkObsWsMsg) where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import qualified Data.ByteString.Lazy.Char8 as LazyBSIO
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.WebSockets as Ws
import System.Environment.Extra (lookupSetting)
import qualified System.Exit as Sys

msgToText :: Ws.DataMessage -> Either String LazyBS.ByteString
msgToText (Ws.Text msg _) = Right msg
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

connectToObsWs :: Ws.Connection -> IO ()
connectToObsWs client = do
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
