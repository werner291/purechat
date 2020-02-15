module API.Rooms where

import Prelude

import API.Core (getJsonAuthed, postJsonAuthed, responseOkOrBust, responseOkWithBody)
import Affjax (URL)
import Data.Argonaut (decodeJson, (.:), (.:?), (:=), (~>))
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Traversable (traverse)
import Effect.Aff (Aff, error, throwError)
import Foreign.Object (Object)
import Global (encodeURIComponent)
import Purechat.Event (EventId, MatrixEvent, MatrixRoomEvent)
import Purechat.Types (PrevBatchToken(..), RoomId(..), SessionInfo, UserId, UserProfile, unPrevBatchToken, unRoomId, unUserId)

-- Short summary of a room's features.
type RoomMeta
  = { room_id :: RoomId
    , canonical_alias :: Maybe String -- Room's canonical alias
    , topic :: Maybe String -- A topic, if the room has one
    , members :: Map UserId UserProfile -- A Map of room participants and their profile (May end up splitting this up with lazy loading)
    , display_name :: String -- Cached version of `roomNameFromData`
    , name :: Maybe String -- Explicitly-set name of the room
    , avatar_url :: Maybe URL
    , pinned_events :: Array EventId
    , tags :: Object Number
    }

tryEncodeUriComponent :: String -> Aff String
tryEncodeUriComponent inp = case encodeURIComponent inp of
    (Just endRid) -> pure endRid
    Nothing -> throwError $ error $ "String " <> inp <> " contains unencodeable characters."

-- Attempt to join a room with a given room ID or alias.
-- Depending on permissions, this may fail.
tryJoinRoom :: SessionInfo -> String -> Aff Unit
tryJoinRoom si rIdOrAlias = do
    encRid <- tryEncodeUriComponent rIdOrAlias
    postJsonAuthed si ("/_matrix/client/r0/join/" <> encRid) JSON.jsonEmptyObject >>= responseOkOrBust

-- Leave a room that the user is in.
leaveRoom :: SessionInfo -> RoomId -> Aff Unit
leaveRoom si rId = do
    encRid <- tryEncodeUriComponent $ unRoomId rId
    postJsonAuthed si ("/_matrix/client/r0/rooms/" <> encRid <> "/leave") JSON.jsonEmptyObject >>= responseOkOrBust

-- Forget a room that the user is in.
forgetRoom :: SessionInfo -> RoomId -> Aff Unit
forgetRoom si rId = do
    encRid <- tryEncodeUriComponent $ unRoomId rId
    postJsonAuthed si ("/_matrix/client/r0/rooms/" <> encRid <> "/forget") JSON.jsonEmptyObject >>= responseOkOrBust

-- Kick a user from a room with a reason.
kickUser :: SessionInfo -> RoomId -> UserId -> String -> Aff Unit
kickUser si rId uId reason = do
    encRid <- tryEncodeUriComponent $ unRoomId rId
    let
        -- Body with message text and message type.
        reqBody :: JSON.Json
        reqBody =
            ( "user_id" := (unUserId uId)
                ~> "reason"
                := reason
                ~> JSON.jsonEmptyObject
            )

        path = "/_matrix/client/r0/rooms/" <> encRid <> "/kick"
    postJsonAuthed si path reqBody >>= responseOkOrBust

-- Ban a user from a room with a reason.
banUser :: SessionInfo -> RoomId -> UserId -> String -> Aff Unit
banUser si rId uId reason = do
    encRid <- tryEncodeUriComponent $ unRoomId rId
    let
        -- Body with message text and message type.
        reqBody :: JSON.Json
        reqBody =
            ( "user_id" := (unUserId uId)
                ~> "reason"
                := reason
                ~> JSON.jsonEmptyObject
            )

        path = "/_matrix/client/r0/rooms/" <> encRid <> "/ban"
    postJsonAuthed si path reqBody >>= responseOkOrBust

-- Unban a user from a room.
unbanUser :: SessionInfo -> RoomId -> UserId -> Aff Unit
unbanUser si rId uId = do
    encRid <- tryEncodeUriComponent $ unRoomId rId
    let
        -- Body with message text and message type.
        reqBody :: JSON.Json
        reqBody =
            ( "user_id" := (unUserId uId)
                ~> JSON.jsonEmptyObject
            )

        path = "/_matrix/client/r0/rooms/" <> encRid <> "/unban"
    postJsonAuthed si path reqBody >>= responseOkOrBust

-- getRoomState :: SessionInfo -> RoomId -> String -> String -> Aff (MatrixEvent MatrixRoomEvent)
-- getRoomState si rId evType stateKey = do
--     encRid <- tryEncodeUriComponent $ unRoomId rId
--     encEvtype <- tryEncodeUriComponent evType
--     encKey <- tryEncodeUriComponent stateKey
--     res <- getJsonAuthed ("/_matrix/client/r0/rooms/" <> encRid <> "/state/" <> encEvtype <> "/" <> encKey) >>= responseOkWithBody
--     case decodeRoomEvent res of
--         Left e -> throwError $ error e
--         Right e -> pure e

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

createRoom :: SessionInfo -> String -> Set UserId -> Aff RoomId
createRoom si alias invitees = do
    let 
        reqBody :: JSON.Json
        reqBody =
            ( 
                "room_alias_name" :=  alias 
                ~> "invite" :=  invitees 
                ~> JSON.jsonEmptyObject 
            )

    json <- responseOkWithBody =<< (postJsonAuthed si "/_matrix/client/r0/createRoom" reqBody)

    case decodeJson json of
        Left err -> throwError $ error err
        Right (dec :: {room_id::String}) -> pure $ RoomId dec.room_id

getEventsUpto :: SessionInfo -> RoomId -> PrevBatchToken -> Aff { from :: Maybe PrevBatchToken
                                                                , to :: Maybe PrevBatchToken
                                                                , chunk :: Array (MatrixEvent MatrixRoomEvent)
                                                                , state :: Array (MatrixEvent MatrixRoomEvent) }
getEventsUpto si rId upto = do 
    encRid <- tryEncodeUriComponent $ unRoomId rId
    res <- getJsonAuthed si ("/_matrix/client/r0/rooms/" <> encRid <>"/messages?from="<>(unPrevBatchToken upto)<>"&dir=b")
    json <- responseOkWithBody res

    let 
        dec = do
            o <- decodeJson json
            chunk <- traverse decodeJson =<< o .: "chunk"
            s_maybe :: Maybe _ <- o .:? "state"
            state <- traverse decodeJson $ (fromMaybe [] s_maybe)
            from <- map PrevBatchToken <$> o .:? "start"
            to <- map PrevBatchToken <$> o .:? "end"
            pure {chunk,state,from,to}

    case dec of
        Left err -> throwError $ error err
        Right r-> pure r
        
