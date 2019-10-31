module Purechat.ServerFeed (serverState, KnownServerState) where

import Prelude

import API.Core (RoomUpdate, SyncPollResult, pollSyncProducer)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Purechat.Types (RoomData, RoomId, SessionInfo, foldEventIntoRoomState)
import Specular.FRP (class MonadFRP, Event, WeakDynamic, changed, filterMapEvent, foldDyn, holdWeakDyn, newEvent)
import Specular.FRP.Async (startAff)

type KnownServerState
  = { joined_rooms :: Map RoomId RoomData
    , invited_to :: Set RoomId
    }

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
