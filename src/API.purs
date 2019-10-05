module API
  ( SessionInfo
  , LoginToken (..)
  , tryLogin
  , unToken
  , remoteResourceGet
  , RemoteResourceStatus (..)
  , stringifyErrors
  , channelList
  , ChannelIndex
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut ((:=), (~>))
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Effect.Aff (Aff, Error)
import Hareactive.Combinators (runAffNow, stepTo)
import Hareactive.Types (Behavior, Now)

type SessionInfo
  = { token :: LoginToken, homeserver :: String }

newtype LoginToken
  = LoginToken String

unToken :: LoginToken -> String
unToken (LoginToken tok) = tok

instance tokenShow :: Show LoginToken where
  show (LoginToken lt) = "API auth token: " <> lt

stringifyErrors :: forall a. Either Error (Either String a) -> (Either String a)
stringifyErrors (Right x) = x

stringifyErrors (Left e) = Left (show e)

tryLogin :: String -> String -> String -> Aff (Either String LoginToken)
tryLogin username password homeserver = do
  let
    reqBody :: JSON.Json
    reqBody =
      ( "type" := "m.login.password"
          ~> "identifier"
          := ("type" := "m.id.user" ~> "user" := username ~> JSON.jsonEmptyObject)
          ~> "password"
          := password
          ~> JSON.jsonEmptyObject
      )
  response <- AX.post AXRF.json (homeserver <> "/_matrix/client/r0/login") (RB.json reqBody)
  pure
    $ case response.status of
        (StatusCode 200) -> case response.body of
          Right json -> case JSON.decodeJson json >>= (\o -> JSON.getField o "access_token") >>= JSON.decodeJson of
            Left err -> Left err
            Right tok -> Right $ LoginToken tok
          _ -> Left "Server returned invalid JSON."
        (StatusCode 401) -> Left "Authentication failed, please verify credentials."
        _ -> Left $ "Unexpected server response HTTP " <> show (response.statusText)

type ChannelIndex
  = { channels :: Array { name :: String }
    }

-- loadChannels :: SessionInfo -> Aff ChannelIndex 
-- loadChannels sess = do
--   response <- AX.get AXRF.json (homeserver <> "/_matrix/client/r0/joined_rooms")
--   let idx = do
--     bodyJson <- decodeJson $ response.body
--     rooms_json <- (JSON.getField bodyJson "joined_rooms" >>= decodeJSON)
--     pure ChannelIndex {
--       rooms: $ decodeJSON <$> rooms_json
--     }
data RemoteResourceStatus a
  = ResourceLoading
  | ResourceLoaded a
  | ResourceError String

instance rrsFunctor :: Functor RemoteResourceStatus where
  map f (ResourceLoaded a) = ResourceLoaded (f a)
  map _ ResourceLoading = ResourceLoading
  map _ (ResourceError e) = ResourceError e

getJSON :: forall a. SessionInfo -> String -> (JSON.Json -> Either String a) -> Aff (Either String a)
getJSON session path decode = do
  resp <- AX.request (AX.defaultRequest { url = (session.homeserver <> path),
     responseFormat = AXRF.json, 
     headers = [RequestHeader "Authorization" ("Bearer " <> ((\(LoginToken tok) -> tok) session.token))] })
  case resp.status of
    (StatusCode 200) -> case resp.body of
      Left e -> pure $ Left $ AX.printResponseFormatError e
      Right jsonBody ->
        let
          x :: Either String a
          x = decode jsonBody
        in
          pure x
    _ -> pure $ Left resp.statusText

remoteResourceGet :: forall a. SessionInfo -> String -> (JSON.Json -> Either String a) -> Now (Behavior (RemoteResourceStatus a))
remoteResourceGet session path decode = do
  responseJSON <- runAffNow $ getJSON session path decode
  let
    eitherToStatus :: Either String a -> RemoteResourceStatus a
    eitherToStatus (Left e) = ResourceError e

    eitherToStatus (Right a) = ResourceLoaded a
  pure $ stepTo ResourceLoading (eitherToStatus <<< stringifyErrors <$> responseJSON)

channelList :: SessionInfo -> Now (Behavior (RemoteResourceStatus ChannelIndex))
channelList si =
  remoteResourceGet si "/_matrix/client/r0/joined_rooms"
    $ \json -> do
        o <- JSON.decodeJson json
        rooms :: Array String <- JSON.getField o "joined_rooms" >>= JSON.decodeJson
        pure
          { channels: { name: _ } <$> rooms
          }
