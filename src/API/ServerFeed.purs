module Purechat.ServerFeed (serverState, KnownServerState) where

import Prelude
import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json, decodeJson, getField, (.:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List.Lazy (foldM)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, throwError)
import Effect.Aff as EE
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Purechat.Types (LoginToken(..), MatrixEvent, MatrixRoomEvent(..), PrevBatchToken(..), RoomData, RoomId(..), SessionInfo, decodeRoomEvent, unRoomId)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, changed, holdWeakDyn, newDynamic, newEvent, subscribeDyn_, subscribeEvent_)
import Specular.FRP.Async (startAff)
import Specular.Internal.Effect (Effect)

-- Approximate implementation of https://matrix.org/docs/spec/client_server/r0.5.0#calculating-the-display-name-for-a-room
roomNameFromData :: RoomId -> RoomData -> String
roomNameFromData rid { name, canonical_alias, members } = fromMaybe (unRoomId rid) $ Array.head $ filterMap identity [ name, canonical_alias ]

updateRoomName :: RoomId -> RoomData -> RoomData
updateRoomName rId rd = rd { display_name = roomNameFromData rId rd }

foldEventIntoRoomState :: RoomId -> RoomData -> MatrixEvent MatrixRoomEvent -> RoomData
foldEventIntoRoomState rId st { content: Right (RoomName name) } = updateRoomName rId $ st { name = Just name }

foldEventIntoRoomState rId st { content: Right (RoomTopic topic) } = updateRoomName rId $ st { topic = Just topic }

foldEventIntoRoomState rId st { content: Right (RoomCanonicalAlias cs) } = updateRoomName rId $ st { canonical_alias = Just cs }

foldEventIntoRoomState rId st { content: Right (Membership m) } = updateRoomName rId $ st { members = Map.insert m.user_id m.profile st.members }

foldEventIntoRoomState rId st _ = st

appendToTimeline :: MatrixEvent MatrixRoomEvent -> RoomData -> RoomData
appendToTimeline ev rd = rd { timeline = Array.snoc rd.timeline ev }

type RoomLeave
  = {}

type RoomInvite
  = {}

type KnownServerState m
  = { joined_rooms :: Map RoomId (Dynamic (RoomData m))
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
  = { new_events :: Array (MatrixEvent MatrixRoomEvent)
    , prev_batch :: PrevBatchToken
    }

decodeRoomUpdate :: Json -> Either String RoomUpdate
decodeRoomUpdate json = do
  o <- decodeJson json
  timeline <- o .: "timeline"
  new_timeline_events <- traverse decodeRoomEvent =<< timeline .: "events"
  prev_batch <- PrevBatchToken <$> timeline .: "prev_batch"
  state <- o .: "state"
  new_state_events <- traverse decodeRoomEvent =<< state .: "events"
  pure
    { new_events: new_timeline_events <> new_state_events
    , prev_batch
    }

-- Essentially a JsonDecode instance for SyncPollResult, 
-- except we currently don't use a custom type for .
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

type RoomFeedState m
  = { onEvent :: MatrixEvent MatrixRoomEvent -> Effect Unit
    , dynamic :: Dynamic Int -> m (Dynamic RoomData)
    }

-- Feed a (Tuple RoomId RoomUpdate) into the feed's internal state.
processJoin :: forall m. MonadFRP m => SessionInfo -> Tuple RoomId RoomUpdate -> Map RoomId (RoomFeedState m) -> Effect (Map RoomId (RoomFeedState m))
processJoin si (Tuple rId ru) st = case (Map.lookup rId st) of
  Just { onEvent } -> do
    for_ ru.new_events $ \evt -> liftEffect $ onEvent evt
    pure st
  Nothing -> do
    -- Dynamic which indicates, for the current room, how many messages should eventually be loaded.
    { dynamic: (minSizeTotal :: Dynamic Int), modify: maxify } <- newDynamic 5
    -- Current room state. Will be updated imperatively.
    { dynamic: (rs_dyn :: Dynamic RoomData)
    , read: (rs_read :: Effect RoomData)
    , set: (rs_set :: RoomData -> Effect Unit)
    } <-
      newDynamic
        { canonical_alias: Nothing
        , display_name: unRoomId rId
        , members: Map.empty
        , name: Nothing
        , timeline: []
        , topic: Nothing
        , from: ru.prev_batch
        , events_requested: false
        }
    let
      -- Handle a single event, folding it into the state of the room.
      onEvent :: MatrixEvent MatrixRoomEvent -> Effect Unit
      onEvent ev = do
        rs <- rs_read
        rs_set $ foldEventIntoRoomState rId rs ev

      -- An effect that returns a dynamic of the roomdata for a given room dynamic.
      -- The roomdata will eventually have at least the requested number of events,
      -- limited by the total number of events.
      viewFromSize :: Dynamic Int -> m (Dynamic RoomData)
      viewFromSize minSize = do
        subscribeDyn_ (\s -> maxify $ \s' -> max s s') minSize
        pure rs_dyn
    pure $ Map.insert rId { onEvent: onEvent, dynamic: viewFromSize } st

type InternalState m
  = { st_rooms :: Map RoomId (RoomFeedState m)
    , st_invites :: Set RoomId
    }

foldSrpStep ::
  forall m.
  MonadFRP m =>
  SessionInfo ->
  SyncPollResult ->
  InternalState m ->
  Effect (InternalState m)
foldSrpStep si spr { st_rooms, st_invites } = do
  let
    new_invites = Set.difference (Set.union st_invites (Map.keys spr.rooms.invite)) (Map.keys spr.rooms.join)
  new_rooms :: Map RoomId (RoomFeedState m) <- (foldM (\a b -> processJoin si b a) st_rooms (Map.toUnfoldable spr.rooms.join))
  pure
    { st_rooms: new_rooms
    , st_invites: new_invites
    }

-- A Dynamic representing the currently known user-relevant state of the Matrix server
-- This data need not be entirely complete, it is simply what is known at the current
-- time, and of that, only the parts that we currently need.
serverState :: forall m. MonadFRP m => SessionInfo -> Dynamic (Map RoomId Int) -> m (WeakDynamic (KnownServerState m))
serverState si = do

  updates :: Event SyncPollResult <- syncFeed si

  { dynamic: st_dyn, read: st_read, set: st_set } <-
    liftEffect $ newDynamic ({ st_rooms: Map.empty, st_invites: Set.empty } :: InternalState m) 

  subscribeEvent_
    ( \(spr :: SyncPollResult) -> do
        st <- st_read
        st' <- foldSrpStep si spr st
        st_set st'
    )
    updates
  holdWeakDyn $ changed (st_dyn <#> \st -> {invited_to: st.st_invites, joined_rooms: (map _.dynamic st.st_rooms)})--       ( st_dyn --           <#> \{ st_rooms, st_invites } -> --               { invited_to: st_invites --               , joined_rooms: (st_rooms <#> _.dynamic) --               } --       ) --       rooms <- liftEffect $ readRef st_rooms --       invites <- liftEffect $ readRef st_invites --       -- TODO --       let --         new_invites = Set.difference (Set.union invites (Map.keys spr.rooms.invite)) (Map.keys spr.rooms.join) --       writeRef st_invites new_invites --       new_rooms :: Map RoomId (RoomFeedState m) <- (foldM (\a b -> processJoin si b a) rooms (Map.toUnfoldable spr.rooms.join)) --       -- writeRef st_rooms rooms --       fire --         { joined_rooms: new_rooms <#> _.dynamic --         , invited_to: invites --         } --   ) --   () -- folded <- foldDyn (\updt rooms -> Just $ performUpdates updt rooms) Nothing feed -- holdWeakDyn $ _.dynamic <$> event --filterMapEvent identity (changed folded) -- type ViewRoom = Dynamic (Maybe String) -> WeakDynamic RoomData