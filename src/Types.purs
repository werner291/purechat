module Purechat.Types where

import Prelude

import Affjax (URL)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

newtype PrevBatchToken
  = PrevBatchToken String

instance showBatchToken :: Show PrevBatchToken where
  show (PrevBatchToken t) = "Batch token " <> t

unPrevBatchToken :: PrevBatchToken -> String
unPrevBatchToken (PrevBatchToken t) = t

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

-------------
-- Profile --
-------------
type UserProfile
  = { displayname :: Maybe String
    , avatar_url :: Maybe URL
    }

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
  currently_active :: Maybe Boolean,
  last_active_ago :: Maybe Int,
  presence :: Presence,
  status_msg :: Maybe String
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