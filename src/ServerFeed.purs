
module Purechat.ServerFeed (serverState) where

import Prelude

import API (RoomUpdate, SyncPollResult, RoomLeave, pollSyncProducer)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Purechat.Types (RoomData, RoomId, SessionInfo, foldEventIntoRoomState)
import Specular.FRP (class MonadFRP, Dynamic, Event, foldDyn, newEvent)
import Specular.FRP.Async (startAff)

-- An Event stream representing poll updates from the Matrix server.
syncFeed :: forall m. MonadFRP m => SessionInfo -> m (Event SyncPollResult) 
syncFeed si = do
    evt <- newEvent
    startAff $ pollSyncProducer si (\update -> liftEffect $ evt.fire update)
    pure evt.event

-- data ServerStateStatus = LoadingState | Loaded (Map RoomId RoomData) | Error String

updateJoins :: Map RoomId RoomUpdate -> Maybe (Map RoomId RoomData) -> Map RoomId RoomData
updateJoins updt rooms = 
    let combineRoom :: RoomUpdate -> Maybe RoomData -> Maybe RoomData--Nothing (Just )
        combineRoom ru Nothing = Just { timeline : {events : ru.new_timeline_events }
                                      , state : foldl foldEventIntoRoomState mempty (ru.new_state_events <> ru.new_timeline_events) }
        combineRoom ru (Just rd) = Just { timeline : {events : rd.timeline.events <> ru.new_timeline_events }
                                   , state : foldl foldEventIntoRoomState rd.state (ru.new_state_events <> ru.new_timeline_events) }
    in foldl (\m (Tuple k ru) -> Map.alter (combineRoom ru) k m) (fromMaybe (Map.empty) rooms) ((Map.toUnfoldable updt) :: Array (Tuple RoomId RoomUpdate))

updateLeaves :: Map RoomId RoomLeave -> Map RoomId RoomData -> Map RoomId RoomData
updateLeaves leave rooms = Map.difference rooms leave

performUpdates :: SyncPollResult -> Maybe (Map RoomId RoomData) -> Map RoomId RoomData
performUpdates srp rooms = updateLeaves srp.rooms.leave $ updateJoins srp.rooms.join $ rooms

-- A Dynamic representing the currently known user-relevant state of the Matrix server
-- This data need not be entirely complete, it is simply what is known at the current
-- time, and of that, only the parts that we currently need.
serverState :: forall m. MonadFRP m => SessionInfo -> m (Dynamic (Maybe (Map RoomId RoomData))) 
serverState si = do
    feed :: Event SyncPollResult <- syncFeed si
    foldDyn (\updt rooms -> Just $ performUpdates updt rooms) Nothing feed