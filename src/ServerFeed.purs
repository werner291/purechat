
module Purechat.ServerFeed (serverState) where

import Prelude

import API (SyncPollResult, pollSyncProducer)
import Data.Map (Map)
import Data.Map as Map
import Effect.Class (liftEffect)
import Purechat.Types (RoomData, RoomId, SessionInfo)
import Specular.FRP (class MonadFRP, Dynamic, Event, foldDyn, newEvent)
import Specular.FRP.Async (startAff)

-- An Event stream representing poll updates from the Matrix server.
syncFeed :: forall m. MonadFRP m => SessionInfo -> m (Event SyncPollResult) 
syncFeed si = do
    evt <- newEvent
    startAff $ pollSyncProducer si (\update -> liftEffect $ evt.fire update)
    pure evt.event

-- A Dynamic representing the currently known user-relevant state of the Matrix server
-- This data need not be entirely complete, it is simply what is known at the current
-- time, and of that, only the parts that we currently need.
serverState :: forall m. MonadFRP m => SessionInfo -> m (Dynamic (Map RoomId RoomData)) 
serverState si = do
    let combine updt rooms = Map.unionWith (<>) rooms (updt.rooms.join)
    feed <- syncFeed si
    foldDyn combine (Map.empty) feed