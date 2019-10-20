module Purechat.Types where

import Prelude

import Data.Argonaut (class DecodeJson, Json, decodeJson, getField, (.:))
import Data.Either (Either(..))

type MatrixEvent a
  = { event_id :: String
    , sender :: String
    , content :: Either String a
    }

class MatrixEventType a where
  eventTypeString :: a -> String

-- data GlobalEventType = MatrixRoomEvent MatrixRoomEvent
-- instance eventTypeStr :: MatrixEventType GlobalEventType where
--   eventTypeString (MatrixRoomEvent e) = eventTypeString e
data MatrixRoomEvent
  = Message { body :: String }

instance matrixRoomEventMatrixEventType :: MatrixEventType MatrixRoomEvent where
  eventTypeString (Message _) = "m.room.message"

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
  let res = case evtType of
        "m.room.message" -> do
          content <- getField o "content"
          body <- getField content "body"
          pure { event_id, sender, content: (Right (Message { body })) }
        _ -> pure { event_id, sender, content: (Left evtType) }
  
  case res of
    Left decodeErr -> pure { event_id, sender, content: (Left decodeErr) }
    Right evt -> Right evt

-- | Record describing all available and known information about a room.
type RoomData
  = { timeline ::
      { events :: Array (MatrixEvent MatrixRoomEvent)
      }
    }

type SessionInfo
  = { token :: LoginToken, homeserver :: String }

newtype LoginToken
  = LoginToken String

unToken :: LoginToken -> String
unToken (LoginToken tok) = tok

instance tokenShow :: Show LoginToken where
  show (LoginToken lt) = "API auth token: " <> lt

------------------
-- RoomId stuff --
------------------
  
newtype RoomId = RoomId String 

unRoomId :: RoomId -> String
unRoomId (RoomId s) = s

derive instance roomIdEq :: Eq RoomId
derive instance roomIdOrd :: Ord RoomId

instance decodeJsonRoomId :: DecodeJson RoomId where
  decodeJson :: Json -> Either String RoomId
  decodeJson jn = do
    o <- decodeJson jn
    pure (RoomId o)