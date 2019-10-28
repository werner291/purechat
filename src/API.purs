module API
  ( tryLogin
  , stringifyErrors
  , pollSyncOnce
  , pollSyncProducer
  , SyncPollResult
  , RoomUpdate
  , RoomInvite
  , RoomLeave
  , sendMessage
  , tryJoinRoom
  , leaveRoom
  , forgetRoom
  , getProfile
  , getMediaWithPossibleMXC
  , UserProfile
  , createRoom
  ) where

import Prelude

import Affjax (Response, ResponseFormatError, URL, printResponseFormatError)
import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AR
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json, decodeJson, getField, getFieldOptional, (.:), (.:?), (:=), (~>))
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.Traversable (traverse)
import Data.UUID as UUID
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Aff as EE
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Global (encodeURIComponent)
import Purechat.Types (LoginToken(..), MatrixEvent, MatrixRoomEvent, RoomId(..), SessionInfo, UserId(..), decodeRoomEvent, unRoomId, unToken, unUserId)
import Web.File.Blob (Blob)

-- | Turns a nested `Either Error (Either String a)` into `Either String a`
-- | by extracting the error message.
stringifyErrors :: forall a. Either Error (Either String a) -> (Either String a)
stringifyErrors (Right x) = x

stringifyErrors (Left e) = Left (show e)

-- | Attempt to obtain a login token using the provided username 
-- | and password pair on the given homeserver uri.
-- | Aff returns either a string error or a Login token.
-- | TODO: This thing is rather dumb and does not take .well-known and such into account.
tryLogin :: String -> String -> String -> Aff SessionInfo
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
  case response.status of
        (StatusCode 200) -> case response.body of
          Right json -> case JSON.decodeJson json of
            Left err -> throwError $ error err
            Right (decoded :: { access_token :: String, user_id :: String }) -> pure  { token: LoginToken decoded.access_token, homeserver: homeserver, user_id: UserId decoded.user_id }
          _ -> throwError $ error "Server returned invalid JSON."
        (StatusCode 401) -> throwError $ error  "Authentication failed, please verify credentials."
        _ -> throwError $ error $ "Unexpected server response HTTP " <> show (response.statusText)

type RoomLeave
  = {}

type RoomInvite
  = {}

-- | The parsed type resulting from a single call to the r0/sync API endpoint.
-- | Consider this type as containing arbitrary new information from the server
-- | that is to be merged with our current view of the server data.
type SyncPollResult
  = { rooms ::
      { join :: Map RoomId RoomUpdate
      , invite :: Map RoomId RoomInvite
      , leave :: Map RoomId RoomLeave
      }
    , next_batch :: String
    }

-- A set of updates specifically pertaining to one room.
type RoomUpdate
  = { new_timeline_events :: Array (MatrixEvent MatrixRoomEvent)
    , new_state_events :: Array (MatrixEvent MatrixRoomEvent)
    }

decodeRoomUpdate :: Json -> Either String RoomUpdate
decodeRoomUpdate json = do
  o <- decodeJson json
  timeline <- o .: "timeline"
  new_timeline_events <- traverse decodeRoomEvent =<< timeline .: "events"
  state <- o .: "state"
  new_state_events <- traverse decodeRoomEvent =<< state .: "events"
  pure
    { new_timeline_events
    , new_state_events
    }

-- Essentially a JsonDecode instance for SyncPollResult, 
-- except we currently don't use a custom type for this.
decodePollResult :: Json -> Either String SyncPollResult
decodePollResult json = do
  let
    objToRoomMap :: forall a. Object a -> Map RoomId a
    objToRoomMap o = foldlWithIndex (\k acc a -> Map.insert (RoomId k) a acc) Map.empty o
  obj <- decodeJson json
  rooms <- obj .: "rooms"
  joinedRooms <- getField rooms "join"
  join :: Map RoomId RoomUpdate <- traverse decodeRoomUpdate $ objToRoomMap joinedRooms
  invitedRooms <- getField rooms "invite"
  invite :: Map RoomId RoomInvite <- traverse decodeJson $ objToRoomMap invitedRooms
  leaveRooms <- getField rooms "leave"
  leave <- traverse decodeJson $ objToRoomMap leaveRooms
  next_batch <- obj .: "next_batch"
  pure { rooms: { join, invite, leave }, next_batch }

pollSyncOnce :: SessionInfo -> Maybe String -> Aff SyncPollResult
pollSyncOnce si since = do
  resp <-
    AX.request
      ( AX.defaultRequest
          { url = (si.homeserver <> "/_matrix/client/r0/sync" <> (Maybe.fromMaybe "?full_state=true" $ map (\x -> "?timeout=30000&since=" <> x) since))
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

authHeader :: SessionInfo -> RequestHeader
authHeader si = RequestHeader "Authorization" ("Bearer " <> ((\(LoginToken tok) -> tok) si.token))

pathToUri :: SessionInfo -> String -> String
pathToUri si path = si.homeserver <> path

-- | Helper method that builds a request with a given session, API endpoint path and optional request body in JSON format.
requestJsonAuthed :: SessionInfo -> String -> Method -> Maybe Json -> Aff (Response (Either ResponseFormatError Json))
requestJsonAuthed si path method body =
  AX.request
    ( AX.defaultRequest
        { url = pathToUri si path
        , responseFormat = AXRF.json
        , headers = [ authHeader si ]
        , method = Left method
        , content = map RB.json body
        }
    )

-- | `requestJsonAuthed` with POST method and required body.
postJsonAuthed :: SessionInfo -> String -> Json -> Aff (Response (Either ResponseFormatError Json))
postJsonAuthed si path body = requestJsonAuthed si path POST (Just body)

-- | `requestJsonAuthed` with PUT method and required body.
putJsonAuthed :: SessionInfo -> String -> Json -> Aff (Response (Either ResponseFormatError Json))
putJsonAuthed si path body = requestJsonAuthed si path PUT (Just body)

-- | `requestJsonAuthed` with GET method and without body.
getJsonAuthed :: SessionInfo -> String -> Aff (Response (Either ResponseFormatError Json))
getJsonAuthed si path = requestJsonAuthed si path GET Nothing

-- | Send a string message into a room with a given ID
sendMessage :: SessionInfo -> RoomId -> String -> Aff Unit
sendMessage si roomId body = do
  -- Generate a message transaction ID to ensure idempotency.
  txnId <- liftEffect $ UUID.genUUID
  let
    -- Body with message text and message type.
    reqBody :: JSON.Json
    reqBody = ("msgtype" := "m.text" ~> "body" := body ~> JSON.jsonEmptyObject)

    -- Path includes room ID, event type and idempotency TXN id.
    path = "/_matrix/client/r0/rooms/" <> (unRoomId roomId) <> "/send/m.room.message/" <> (UUID.toString txnId)
  postJsonAuthed si path reqBody >>= responseOkOrBust

-- | Simplify a response with possible error status into an Aff that returns Unit
-- | if the response status is 200.
responseOkOrBust :: Response (Either ResponseFormatError Json) -> Aff Unit
responseOkOrBust resp = case resp.status of
  (StatusCode 200) -> pure unit
  _ -> throwError (error resp.statusText)

-- | Simplify a response that might not have a success response and decoding issues
-- | into a simple Aff that returns the body upon success, and throws any errors
-- | that may pop up as an `Error`.
responseOkWithBody :: forall b. Response (Either ResponseFormatError b) -> Aff b
responseOkWithBody resp = case resp.status of
  (StatusCode 200) -> case resp.body of
    Left e -> throwError (error $ printResponseFormatError e)
    Right b -> pure b
  _ -> throwError (error resp.statusText)

-- Attempt to join a room with a given room ID or alias.
-- Depending on permissions, this may fail.
tryJoinRoom :: SessionInfo -> String -> Aff Unit
tryJoinRoom si rIdOrAlias = postJsonAuthed si ("/_matrix/client/r0/rooms/" <> rIdOrAlias <> "/join") JSON.jsonEmptyObject >>= responseOkOrBust

-- Leave a room that the user is in.
leaveRoom :: SessionInfo -> RoomId -> Aff Unit
leaveRoom si rId = postJsonAuthed si ("/_matrix/client/r0/rooms/" <> (unRoomId rId) <> "/leave") JSON.jsonEmptyObject >>= responseOkOrBust

-- Forget a room that the user is in.
forgetRoom :: SessionInfo -> RoomId -> Aff Unit
forgetRoom si rId = postJsonAuthed si ("/_matrix/client/r0/rooms/" <> (unRoomId rId) <> "/forget") JSON.jsonEmptyObject >>= responseOkOrBust

-- Kick a user from a room with a reason.
kickUser :: SessionInfo -> RoomId -> UserId -> String -> Aff Unit
kickUser si rId uId reason =
  let
    -- Body with message text and message type.
    reqBody :: JSON.Json
    reqBody =
      ( "user_id" := (unUserId uId)
          ~> "reason"
          := reason
          ~> JSON.jsonEmptyObject
      )

    path = "/_matrix/client/r0/rooms/" <> (unRoomId rId) <> "/kick"
  in
    postJsonAuthed si path reqBody >>= responseOkOrBust

-- Ban a user from a room with a reason.
banUser :: SessionInfo -> RoomId -> UserId -> String -> Aff Unit
banUser si rId uId reason =
  let
    -- Body with message text and message type.
    reqBody :: JSON.Json
    reqBody =
      ( "user_id" := (unUserId uId)
          ~> "reason"
          := reason
          ~> JSON.jsonEmptyObject
      )

    path = "/_matrix/client/r0/rooms/" <> (unRoomId rId) <> "/ban"
  in
    postJsonAuthed si path reqBody >>= responseOkOrBust

-- Unban a user from a room.
unbanUser :: SessionInfo -> RoomId -> UserId -> Aff Unit
unbanUser si rId uId =
  let
    -- Body with message text and message type.
    reqBody :: JSON.Json
    reqBody =
      ( "user_id" := (unUserId uId)
          ~> JSON.jsonEmptyObject
      )

    path = "/_matrix/client/r0/rooms/" <> (unRoomId rId) <> "/unban"
  in
    postJsonAuthed si path reqBody >>= responseOkOrBust

-- | Perform a GET request for the given URL
-- | This function automatically handles urls that use the mxc:// scheme (hence why it needs a session),
-- | and passes the other URLs on to Affjax to be handled.
getMediaWithPossibleMXC :: SessionInfo -> URL -> Aff Blob
getMediaWithPossibleMXC si url = do
  case (String.stripPrefix (String.Pattern "mxc://") url) of
    Just mxc -> getMXC si mxc
    Nothing -> AX.get AR.blob url >>= responseOkWithBody

-- | Fetch an MXC resource.
getMXC :: SessionInfo -> String -> Aff Blob
getMXC si mediaId = do
  resp <-
    AX.request
      ( AX.defaultRequest
          { url = pathToUri si ("/_matrix/client/r0/download/" <> mediaId)
          , responseFormat = AXRF.blob
          , headers = [ authHeader si ]
          , method = Left GET
          }
      )
  responseOkWithBody resp

type UserProfile
  = { displayname :: String, avatar_url :: Maybe URL }

getProfile :: SessionInfo -> UserId -> Aff UserProfile
getProfile si uid = case encodeURIComponent $ unUserId uid of
  Just encUid -> do
    json <- responseOkWithBody =<< (getJsonAuthed si $ "/_matrix/client/r0/profile/" <> encUid)
    let
      decoded = do
        o <- decodeJson json
        displayname :: String <- o .: "displayname"
        avatar_url :: Maybe String <- getFieldOptional o "avatar_url"
        pure { displayname, avatar_url }
    case decoded of
      Right x -> pure x
      Left e -> throwError $ error e
  Nothing -> throwError $ error "UserId contains unencodeable characters."

-- If the given URL is one that starts with mxc://
-- this function returns an https URL with a proper access token as a URL parameter.
-- Otherwise, this function is the identity function.
httpOrMxcToHttp :: SessionInfo -> URL -> URL
httpOrMxcToHttp si url = case (String.stripPrefix (String.Pattern "mxc://") url) of
  Just mediaId -> pathToUri si ("/_matrix/client/r0/download/" <> (mediaId) <> "?access_token=" <> (unToken si.token))
  Nothing -> url

type DirectoryEntry
  = { room_id :: RoomId }

type DirectoryView
  = { chunk :: Array DirectoryEntry
    , next_batch :: Maybe String
    , prev_batch :: Maybe String
    , total_room_count_estimate :: Maybe Int
    }

getDirectory :: SessionInfo -> Maybe String -> Maybe String -> Maybe String -> Aff DirectoryView
getDirectory si filter from to = do
  json <- responseOkWithBody =<< getJsonAuthed si "/_matrix/client/r0/publicRooms"

  let 
    decoded :: Either String DirectoryView 
    decoded = do
      o <- decodeJson json
      chunk <- o .: "chunk"
      next_batch <- o .:? "next_batch"
      prev_batch <- o .:? "prev_batch"
      total_room_count_estimate <- o .:? "total_room_count_estimate"
      pure { chunk, next_batch, prev_batch, total_room_count_estimate }

  case decoded of
    Left err -> throwError $ error err
    Right dec -> pure dec

createRoom :: SessionInfo -> String -> Aff RoomId
createRoom si alias = do
  let 
    reqBody :: JSON.Json
    reqBody =
      ( 
        "room_alias_name" :=  alias ~> JSON.jsonEmptyObject 
      )

  json <- responseOkWithBody =<< (postJsonAuthed si "/_matrix/client/r0/createRoom" reqBody)

  case decodeJson json of
    Left err -> throwError $ error err
    Right (dec :: {room_id::String}) -> pure $ RoomId dec.room_id
