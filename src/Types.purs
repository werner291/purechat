module Purechat.Types where

import Prelude

import Affjax (URL)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, getField, getFieldOptional, getFieldOptional', stringify, (.:))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.Set (Set)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Specular.FRP (Dynamic)

newtype EventId
  = MkEventId String

derive instance newtypeEventId :: Newtype EventId _

derive newtype instance eqEventId :: Eq EventId

derive newtype instance ordEventId :: Ord EventId

-- Combination of milliseconds since unix epoch and event id.
data TimeEventId
  = TimeEventId Milliseconds EventId

derive instance eqTimeEventId :: Eq TimeEventId

instance ordTimeEventId :: Ord TimeEventId where
  compare (TimeEventId ta ea) (TimeEventId tb eb) = case compare ta tb of
    EQ -> compare ea eb
    c -> c

type MatrixEvent a
  = { event_id :: EventId
    , sender :: UserId
    , content :: Either String a
    , origin_server_ts :: Milliseconds
    }

getTimeId :: forall a. MatrixEvent a -> TimeEventId
getTimeId ev = TimeEventId ev.origin_server_ts ev.event_id

class MatrixEventType a where
  eventTypeString :: a -> String

data RoomMembership
  = Join
  | Leave
  | Invite

instance decodeRoomMembership :: DecodeJson RoomMembership where
  decodeJson :: Json -> Either String RoomMembership
  decodeJson json = do
    t <- decodeJson json
    case t of
      "join" -> Right Join
      "leave" -> Right Leave
      "invite" -> Right Invite
      _ -> Left $ "Unknown join status: " <> t

data MessageType
  = Text
  -- | Emote
  -- | Notice
  -- | Image URL
  -- | File URL
  -- | Audio
  -- | Location String
  -- | Video URL
  | Unknown String

data MatrixRoomEvent
  = Message { body :: String, msgtype :: MessageType }
  | Membership { profile :: UserProfile, membership :: RoomMembership, user_id :: UserId }
  | RoomName String
  | RoomTopic String
  | RoomCanonicalAlias String
  | RoomAvatar URL
  | RoomPinnedEvents (Array EventId)

-- | Decode a Json object as a room event.
-- | Note that, if the event type is unrecognized or the "content" field of the event
-- | fails to decode, an event with a Left "content" field is generated to stop individual
-- | events from causing the entire decode to fail.
decodeRoomEvent :: Json -> Either String (MatrixEvent MatrixRoomEvent)
decodeRoomEvent json = do
  o <- decodeJson json
  event_id <- wrap <$> o .: "event_id"
  sender <- o .: "sender"
  evtType <- o .: "type"
  origin_server_ts <- Milliseconds <$> o .: "origin_server_ts"
  state_key :: String <- fromMaybe "" <$> getFieldOptional o "state_key"
  let
    res = case evtType of
      "m.room.message" -> do
        content <- getField o "content"
        body <- getField content "body"
        msg_type_txt <- getField content "msgtype"
        case msg_type_txt of
          "m.text" -> pure $ Message { body, msgtype: Text }
          _ -> pure $ Message { body, msgtype: Unknown msg_type_txt}
        
      "m.room.member" -> do
        content <- getField o "content"
        displayname <- getFieldOptional' content "displayname"
        avatar_url <- getFieldOptional' content "avatar_url"
        membership <- getField content "membership"
        pure $ Membership { profile: { displayname, avatar_url }, membership, user_id: UserId state_key }
      "m.room.name" -> do
        content <- getField o "content"
        name <- getField content "name"
        pure $ RoomName name
      "m.room.topic" -> do
        content <- getField o "content"
        topic <- getField content "topic"
        pure $ RoomTopic topic
      "m.room.canonical_alias" -> do
        content <- getField o "content"
        ca <- getField content "alias"
        pure $ RoomCanonicalAlias ca
      "m.room.avatar" -> do
        url <- getField o "url"
        pure $ RoomAvatar url
      _ -> throwError $ "Unknown event type " <> evtType
  case res of
    Left decodeErr -> pure { origin_server_ts, event_id, sender, content: (Left $ decodeErr <> (stringify json)) }
    Right evt -> Right { origin_server_ts, event_id, sender, content: Right evt }

newtype PrevBatchToken
  = PrevBatchToken String

instance showBatchToken :: Show PrevBatchToken where
  show (PrevBatchToken t) = "Batch token " <> t

unPrevBatchToken :: PrevBatchToken -> String
unPrevBatchToken (PrevBatchToken t) = t

-- | Record describing all available and known information about a room
-- type Room
--   = { timeline :: Array (MatrixEvent MatrixRoomEvent) -- May eventually be replaced with a more high-level view of the room
--     , name :: Maybe String -- Explicitly-set name of the room
--     , canonical_alias :: Maybe String -- Room's canonical alias
--     , topic :: Maybe String -- A topic, if the room has one
--     , members :: Map UserId UserProfile -- A Map of room participants and their profile
--     , display_name :: String -- Cached version of `roomNameFromData`
--     , from :: PrevBatchToken -- The timestamp upto which events are incorporated into the RoomData
--     , events_requested :: Boolean -- Whether the backend is currently fetching more events
--     }
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
  = { displayname :: Maybe String
    , avatar_url :: Maybe URL
    }

-- appendToTimeline :: MatrixEvent MatrixRoomEvent -> RoomData -> RoomData
-- appendToTimeline ev rd = rd { timeline = Array.snoc rd.timeline ev }
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
    }

-- A bundle of Dynamics related to a given room.
type JoinedRoom m
  = { messages :: Dynamic Int -> m (Dynamic (Array (MatrixEvent MatrixRoomEvent)))
    , loadingMessages :: Dynamic Boolean
    , meta :: Dynamic RoomMeta
    }

-- dynState :: forall m. MonadFRP m => m (Dynamic Map RoomId (JoinedRoom m))
type KnownServerState m
  = { joined_rooms :: Dynamic (Map RoomId (JoinedRoom m))
    , invited_to :: Dynamic (Set RoomId)
    , global_presence :: Dynamic (Map UserId (Dynamic UserStatus))
    }

type RoomLeave
  = {}

type RoomInvite
  = {}

data Presence = Online | Offline | Unavailable

instance decodePresence :: DecodeJson Presence where
  decodeJson json = decodeJson json >>= case _ of
    "offline" -> pure Offline
    "online" -> pure Online
    "unavailable" -> pure Unavailable
    other -> throwError $ "Unknown presence value " <> other

type UserStatus = {
  avatar_url :: Maybe URL,
  displayname :: Maybe String,
  currently_active :: Boolean,
  last_active_ago :: Int,
  presence :: Presence,
  status_msg :: Maybe String
}

-- | The parsed type resulting from a single call to the r0/sync API endpoint.
-- | Consider this type as containing arbitrary new information from the server
-- | that is to be merged with our current view of the server data.
type SyncPollResult
  = { rooms ::
      { join :: Map RoomId RoomUpdate
      , invite :: Map RoomId RoomInvite
      , leave :: Map RoomId RoomLeave
      }
    , presence :: Map UserId UserStatus
    , next_batch :: String
    }

-- A set of updates specifically pertaining to one room.
type RoomUpdate
  = { new_timeline_events :: Array (MatrixEvent MatrixRoomEvent)
    , new_state_events :: Array (MatrixEvent MatrixRoomEvent)
    , prev_batch :: PrevBatchToken
    }

-- Similar to Maybe, but more semantically precise way
-- of signaling that a resource is either loading or currently available.
data RemoteResourceView a
  = RRLoading
  | RRLoaded a

instance functorRRView :: Functor RemoteResourceView where
  map f (RRLoading) = RRLoading
  map f (RRLoaded a) = RRLoaded (f a)

fromRRLoaded :: forall a. RemoteResourceView a -> Maybe a
fromRRLoaded (RRLoaded a) = Just a
fromRRLoaded _ = Nothing

-----------------
-- Environment --
-- Note: The GlovalEnv must store only things that are either globally relevant within the session
-- or cannot be dealt with deep within the view hierarchy.
-- It may be necessary to split this dictionary into more local environments.
-----------------

type GlobalEnv m = 
  { showProfile :: UserId -> Maybe UserProfile -> Effect Unit
  , session :: SessionInfo -- Deprecated, use dedicated functions where possible.
  , user_profile :: Dynamic (Maybe UserProfile)
  , channels_state :: Dynamic (RemoteResourceView (KnownServerState m))
  , logout :: Effect Unit
  , editProfile :: Effect Unit
  , openRoom :: RoomId -> Effect Unit
  , createRoom :: Effect Unit
  , closeCurrentProfileCard :: Effect Unit
  }