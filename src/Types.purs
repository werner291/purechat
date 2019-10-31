module Purechat.Types where

import Prelude

import Affjax (URL)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, getField, getFieldOptional, (.:))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)

type MatrixEvent a
  = { event_id :: String
    , sender :: String
    , content :: Either String a
    }

class MatrixEventType a where
  eventTypeString :: a -> String

data RoomMembership
  = Joined

instance decodeRoomMembership :: DecodeJson RoomMembership where
  decodeJson :: Json -> Either String RoomMembership
  decodeJson json = do
    t <- decodeJson json
    case t of
      "join" -> Right Joined
      _ -> Left $ "Unknown join status: " <> t

-- data GlobalEventType = MatrixRoomEvent MatrixRoomEvent
-- instance eventTypeStr :: MatrixEventType GlobalEventType where
--   eventTypeString (MatrixRoomEvent e) = eventTypeString e
data MatrixRoomEvent
  = Message { body :: String }
  | Membership { profile :: UserProfile, membership :: RoomMembership, user_id :: UserId }
  | RoomName String
  | RoomTopic String
  | RoomCanonicalAlias String

instance matrixRoomEventMatrixEventType :: MatrixEventType MatrixRoomEvent where
  eventTypeString (Message _) = "m.room.message"
  eventTypeString (Membership _) = "m.room.membership"
  eventTypeString (RoomName _) = "m.room.name"
  eventTypeString (RoomTopic _) = "m.room.topic"
  eventTypeString (RoomCanonicalAlias _) = "m.room.canonical_alias"

-- | Decode a Json object as a room event.
-- | Note that, if the event type is unrecognized or the "content" field of the event
-- | fails to decode, an event with a Left "content" field is generated to stop individual
-- | events from causing the entire decode to fail.
decodeRoomEvent :: Json -> Either String (MatrixEvent MatrixRoomEvent)
decodeRoomEvent json = do
  o <- decodeJson json
  event_id <- o .: "event_id"
  sender <- o .: "sender"
  evtType <- o .: "type"
  state_key :: String <- fromMaybe "" <$> getFieldOptional o "state_key"
  let
    res = case evtType of
      "m.room.message" -> do
        content <- getField o "content"
        body <- getField content "body"
        pure { event_id, sender, content: (Right (Message { body })) }
      "m.room.member" -> do
        content <- getField o "content"
        displayname <- getField content "displayname"
        avatar_url <- getFieldOptional content "avatar_url"
        membership <- getField content "membership"
        
        pure { event_id, sender, content: (Right (Membership { profile:{displayname, avatar_url}, membership, user_id: UserId state_key })) }
      "m.room.name" -> do
        content <- getField o "content"
        name <- getField content "name"
        pure { event_id, sender, content: (Right $ RoomName name) }
      "m.room.topic" -> do
        content <- getField o "content"
        topic <- getField content "topic"
        pure { event_id, sender, content: (Right $ RoomTopic topic) }
      "m.room.canonical_alias" -> do
        content <- getField o "content"
        ca <- getField content "alias"
        pure { event_id, sender, content: (Right $ RoomCanonicalAlias ca) }
      _ -> pure { event_id, sender, content: (Left evtType) }
  case res of
    Left decodeErr -> pure { event_id, sender, content: (Left decodeErr) }
    Right evt -> Right evt

-- | Record describing all available and known information about a room.
type RoomData
  = { timeline :: Array (MatrixEvent MatrixRoomEvent)
    , name :: Maybe String
    , canonical_alias :: Maybe String
    , topic :: Maybe String
    , members :: Map UserId UserProfile
    , display_name :: String -- Cached version of `roomNameFromData`
    }

type SessionInfo
  = { token :: LoginToken, homeserver :: String, user_id :: UserId }

newtype LoginToken
  = LoginToken String

unToken :: LoginToken -> String
unToken (LoginToken tok) = tok

instance tokenShow :: Show LoginToken where
  show (LoginToken lt) = "API auth token: " <> lt

instance decodeToken :: DecodeJson LoginToken where
  decodeJson :: Json -> Either String LoginToken
  decodeJson j = do
    t :: String <- decodeJson j
    pure (LoginToken t)

instance encodeToken :: EncodeJson LoginToken where
  encodeJson (LoginToken t) = encodeJson t

------------------
-- RoomId stuff --
------------------
newtype RoomId
  = RoomId String

unRoomId :: RoomId -> String
unRoomId (RoomId s) = s

mkRoomId :: String -> RoomId
mkRoomId s = RoomId s

derive instance roomIdEq :: Eq RoomId

derive instance roomIdOrd :: Ord RoomId

instance decodeJsonRoomId :: DecodeJson RoomId where
  decodeJson :: Json -> Either String RoomId
  decodeJson jn = do
    o <- decodeJson jn
    pure (RoomId o)

newtype UserId
  = UserId String

unUserId :: UserId -> String
unUserId (UserId s) = s

derive instance userIdEq :: Eq UserId

derive instance userIdOrd :: Ord UserId

instance decodeJsonUserId :: DecodeJson UserId where
  decodeJson jn = do
    o <- decodeJson jn
    pure (UserId o)

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson (UserId u) = encodeJson u

-------------
-- Profile --
-------------
type UserProfile
  = { displayname :: String, avatar_url :: Maybe URL }
