module Purechat.ServerFeed (serverState, KnownServerState) where

import Prelude

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json, decodeJson, getField, (.:))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, throwError)
import Effect.Aff as EE
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Purechat.Types (LoginToken(..), MatrixEvent, MatrixRoomEvent, RoomData, RoomId(..), SessionInfo, decodeRoomEvent, foldEventIntoRoomState)
import Specular.FRP (class MonadFRP, Event, WeakDynamic, changed, filterMapEvent, foldDyn, holdWeakDyn, newEvent)
import Specular.FRP.Async (startAff)

type RoomLeave
  = {}

type RoomInvite
  = {}

type KnownServerState
  = { joined_rooms :: Map RoomId RoomData
    , invited_to :: Set RoomId
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

-- An Event stream representing poll updates from the Matrix server.
syncFeed :: forall m. MonadFRP m => SessionInfo -> m (Event SyncPollResult)
syncFeed si = do
  evt <- newEvent
  startAff $ pollSyncProducer si (\update -> liftEffect $ evt.fire update)
  pure evt.event

-- data ServerStateStatus = LoadingState | Loaded (Map RoomId RoomData) | Error String
updateJoins :: SyncPollResult -> Maybe KnownServerState -> KnownServerState
updateJoins updt st =
  let
    combineRoom :: RoomUpdate -> Maybe RoomData -> Maybe RoomData --Nothing (Just )
    combineRoom ru Nothing =
      Just
        { timeline: { events: ru.new_timeline_events }
        , state: foldl foldEventIntoRoomState mempty (ru.new_state_events <> ru.new_timeline_events)
        }

    combineRoom ru (Just rd) =
      Just
        { timeline: { events: rd.timeline.events <> ru.new_timeline_events }
        , state: foldl foldEventIntoRoomState rd.state (ru.new_state_events <> ru.new_timeline_events)
        }

    foldTuple :: KnownServerState -> Tuple RoomId RoomUpdate -> KnownServerState
    foldTuple m (Tuple k ru) = m { joined_rooms = Map.alter (combineRoom ru) k m.joined_rooms }

    update_tuples :: Array (Tuple RoomId RoomUpdate)
    update_tuples = Map.toUnfoldable updt.rooms.join
  in
    foldl foldTuple (fromMaybe mempty st) update_tuples

updateLeaves :: SyncPollResult -> KnownServerState -> KnownServerState
updateLeaves srp s = s { joined_rooms = Map.difference s.joined_rooms srp.rooms.leave }

updateInvites :: SyncPollResult -> KnownServerState -> KnownServerState
updateInvites srp st = st { invited_to = 
  let 
    with_new_invites = Set.union (st.invited_to) (Map.keys srp.rooms.invite)
  in Set.difference with_new_invites (Map.keys srp.rooms.join) }

performUpdates :: SyncPollResult -> Maybe KnownServerState -> KnownServerState
performUpdates srp s = updateLeaves srp $ updateInvites srp $ updateJoins srp $ s

-- A Dynamic representing the currently known user-relevant state of the Matrix server
-- This data need not be entirely complete, it is simply what is known at the current
-- time, and of that, only the parts that we currently need.
serverState :: forall m. MonadFRP m => SessionInfo -> m (WeakDynamic KnownServerState)
serverState si = do
  feed :: Event SyncPollResult <- syncFeed si
  folded <- foldDyn (\updt rooms -> Just $ performUpdates updt rooms) Nothing feed
  holdWeakDyn $ filterMapEvent identity (changed folded)
