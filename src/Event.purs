module Purechat.Event where

import Prelude

import Affjax (URL)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, getField, getFieldOptional', stringify, (.:))
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Foreign.Object (Object)
import Purechat.Types (RoomMembership, UserId, UserProfile)

-- Event Id --
newtype EventId
  = MkEventId String

derive instance newtypeEventId :: Newtype EventId _

derive newtype instance eqEventId :: Eq EventId

derive newtype instance ordEventId :: Ord EventId

instance decodeJsonEventId :: DecodeJson EventId where
  decodeJson json = map wrap $ decodeJson json

instance encodeJsonEventId :: EncodeJson EventId where
  encodeJson eId = encodeJson (unwrap eId)

-- class MatrixEventType a where
--   eventTypeString :: a -> String
-- For events of type "m.room.message", decode their inner content according to their "msgtype" field.
data MessageType
  = Text
  | Emote
  | Notice
  | Image URL
  | File URL
  | Audio URL
  | Location String
  | Video URL
  | Unknown String

data MatrixRoomEvent
  = Message { body :: String, msgtype :: MessageType }
  | Membership { profile :: UserProfile, membership :: RoomMembership, user_id :: UserId }
  | RoomName String
  | RoomTopic String
  | RoomCanonicalAlias String
  | RoomAvatar URL
  | RoomPinnedEvents (Array EventId)
  | UnknownRoomEvent String

decodeRoomEventContent :: Object Json -> Either String MatrixRoomEvent
decodeRoomEventContent o = do
  content <- o .: "content"
  o .: "type" >>= case _ of

    -- A regular message, possibly containing multimedia based on its' msgType member.
    "m.room.message" -> do
      body <- content .: "body"
      msgtype <-
        content .: "msgtype"
          >>= case _ of
              "m.text" -> pure Text
              "m.emote" -> pure Emote
              "m.notice" -> pure Notice -- TODO verify we're using the right keys here.
              "m.image" -> Image <$> o .: "url"
              "m.file" -> File <$> o .: "url"
              "m.audio" -> Audio <$> o .: "url"
              "m.location" -> Location <$> o .: "url"
              "m.video" -> Video <$> o .: "url"
              unknown -> pure $ Unknown unknown
      pure $ Message { body, msgtype }
      
    -- Room membership, for when a user enters/leaves a room or changes their status while in the room.
    "m.room.member" -> do
      displayname <- getFieldOptional' content "displayname"
      avatar_url <- getFieldOptional' content "avatar_url"
      membership <- content .: "membership"
      -- State key determines whose status this affects.
      state_key <- o .: "state_key"
      pure $ Membership { profile: { displayname, avatar_url }, membership, user_id: state_key }

    -- Room display name update
    "m.room.name" -> do
      name <- getField content "name"
      pure $ RoomName name
    
    -- Room topic update
    "m.room.topic" -> do
      topic <- getField content "topic"
      pure $ RoomTopic topic

    -- Room's canonical alias.
    "m.room.canonical_alias" -> do
      ca <- getField content "alias"
      pure $ RoomCanonicalAlias ca

    -- Room's avatar.
    "m.room.avatar" -> do
      url <- getField content "url"
      pure $ RoomAvatar url

    -- Event types can be made up on the fly, so an unknown one isn't too exceptional.
    unknown -> pure $ UnknownRoomEvent $ "Unknown event type " <> unknown

instance decodeJsonRoomEvent :: DecodeJson (MatrixEvent MatrixRoomEvent) where
  decodeJson json = do
    o <- decodeJson json
    -- These fields are common to all room events.
    event_id <- o .: "event_id"
    sender <- o .: "sender"
    -- Not using monadic bind because we keep any errors as Left in the resulting value.
    let content = case decodeRoomEventContent o of
          (Right c) -> Right c
          (Left e) -> Left (e <> " Original: " <> stringify json)
    origin_server_ts <- Milliseconds <$> o .: "origin_server_ts"
    
    pure $ MatrixEvent {
      content, event_id, origin_server_ts, sender
    }
    
-- newtype PresenceEvent 

---------------------------------
-- Time-Event id helper tuple. --
---------------------------------
-- beginregion
-- Combination of milliseconds since unix epoch and event id.
data TimeEventId
  = TimeEventId Milliseconds EventId

derive instance eqTimeEventId :: Eq TimeEventId

instance ordTimeEventId :: Ord TimeEventId where
  compare (TimeEventId ta ea) (TimeEventId tb eb) = case compare ta tb of
    EQ -> compare ea eb
    c -> c

newtype MatrixEvent a
  = MatrixEvent
  { event_id :: EventId
  , sender :: UserId
  , content :: Either String a
  , origin_server_ts :: Milliseconds
  }

derive instance newtypeMatrixEvent :: Newtype (MatrixEvent a) _

getTimeId :: forall a. MatrixEvent a -> TimeEventId
getTimeId (MatrixEvent ev) = TimeEventId ev.origin_server_ts ev.event_id
