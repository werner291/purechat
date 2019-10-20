module API
  ( tryLogin
  , stringifyErrors
  , pollSyncOnce
  , pollSyncProducer
  , SyncPollResult
  -- , longpollStream
  -- , flattenArrayStream
  , sendMessage
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json, decodeJson, getField, (.:), (:=), (~>))
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.UUID as UUID
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Aff as EE
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Purechat.Types (LoginToken(..), RoomData, RoomId(..), SessionInfo, decodeRoomEvent, unRoomId)

-- | Turns a nested `Either Error (Either String a)` into `Either String a`
-- | by extracting the error message.
stringifyErrors :: forall a. Either Error (Either String a) -> (Either String a)
stringifyErrors (Right x) = x
stringifyErrors (Left e) = Left (show e)

-- | Attempt to obtain a login token using the provided username 
-- | and password pair on the given homeserver uri.
-- | Aff returns either a string error or a Login token.
-- | TODO: This thing is rather dumb and does not take .well-known and such into account.
tryLogin :: String -> String -> String -> Aff (Either String SessionInfo)
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
            Right tok -> Right $ {token:LoginToken tok, homeserver:homeserver}
          _ -> Left "Server returned invalid JSON."
        (StatusCode 401) -> Left "Authentication failed, please verify credentials."
        _ -> Left $ "Unexpected server response HTTP " <> show (response.statusText)

-- | The parsed type resulting from a single call to the r0/sync API endpoint.
-- | Consider this type as containing arbitrary new information from the server
-- | that is to be merged with our current view of the server data.
type SyncPollResult
  = { rooms ::
      { join :: Map RoomId RoomData
      }
    , next_batch :: String
    }

decodeRoom :: Json -> Either String RoomData
decodeRoom json = do
  o <- decodeJson json
  timeline <- o .: "timeline"
  events_json <- timeline .: "events"
  events <- traverse decodeRoomEvent events_json
  pure
    { timeline: { events }
    }

-- Essentially a JsonDecode instance for SyncPollResult, 
-- except we currently don't use a custom type for this.
decodePollResult :: Json -> Either String SyncPollResult
decodePollResult json = do
  obj <- decodeJson json
  rooms <- obj .: "rooms"
  joinedRooms <- getField rooms "join"
  decJoinedRooms :: Map RoomId RoomData <- traverse decodeRoom $ foldlWithIndex (\k acc a -> Map.insert (RoomId k) a acc) Map.empty (joinedRooms :: Object Json)
  next_batch <- obj .: "next_batch"
  pure { rooms: { join: decJoinedRooms }, next_batch }


pollSyncOnce :: SessionInfo -> Maybe String -> Aff SyncPollResult
pollSyncOnce si since = do
  resp <-
    AX.request
      ( AX.defaultRequest
          { url = (si.homeserver <> "/_matrix/client/r0/sync" <> (Maybe.fromMaybe "" $ map (\x -> "?timeout=30000&since=" <> x) since))
          , responseFormat = AXRF.json
          , headers = [ RequestHeader "Authorization" ("Bearer " <> ((\(LoginToken tok) -> tok) si.token)) ]
          }
      )
  case resp.status of
    (StatusCode 200) -> case resp.body of
      Left e -> throwError $ EE.error (AX.printResponseFormatError e)
      Right jsonBody -> case decodePollResult jsonBody of
        Left e -> throwError $ EE.error e
        Right x -> pure x
    _ -> throwError $ EE.error resp.statusText

pollSyncProducer :: SessionInfo -> (SyncPollResult -> Aff Unit) -> Aff Unit
pollSyncProducer si callback = 
  let 
    pollLoop :: Maybe String -> Aff Unit
    pollLoop since = do
      res <- pollSyncOnce si since 
      callback res
      pollLoop $ Just res.next_batch
  in
    pollLoop Nothing

-- Send a string message into a room with a given ID
sendMessage :: SessionInfo -> RoomId -> String -> Aff Unit
sendMessage si roomId body = do

  txnId <- liftEffect $ UUID.genUUID

  let
    reqBody :: JSON.Json
    reqBody = ("msgtype" := "m.text" ~> "body" := body ~> JSON.jsonEmptyObject)

    url :: String
    url =
      si.homeserver
        <> "/_matrix/client/r0/rooms/"
        <> (unRoomId roomId)
        <> "/send/m.room.message/"
        <> (UUID.toString txnId)
  
  resp <- AX.request 
          ( AX.defaultRequest
              { url = url
              , responseFormat = AXRF.json
              , headers = [ RequestHeader "Authorization" ("Bearer " <> ((\(LoginToken tok) -> tok) si.token)) ]
              , method = Left PUT
              , content = Just (RB.json reqBody)
              }
          )
  
  case resp.status of
    (StatusCode 200) -> pure unit
    _ -> throwError (error resp.statusText)
