module Purechat.ServerFeed (serverState, KnownServerState, JoinedRoom, RoomMeta) where

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
import Purechat.Types (LoginToken(..), MatrixEvent, MatrixRoomEvent(..), PrevBatchToken(..), RoomId(..), SessionInfo, UserId, UserProfile, decodeRoomEvent, unRoomId)
import Specular.FRP (class MonadFRP, Dynamic, Event, InnerFRP, WeakDynamic, changed, filterMapEvent, foldDyn, holdWeakDyn, newEvent)
import Specular.FRP.Async (startAff)
import Specular.FRP.Base (foldDynM)

-- Approximate implementation of https://matrix.org/docs/spec/client_server/r0.5.0#calculating-the-display-name-for-a-room
roomNameFromData :: RoomMeta -> String
roomNameFromData { room_id, name, canonical_alias, members } = fromMaybe (unRoomId room_id) $ Array.head $ filterMap identity [ name, canonical_alias ]

updateRoomName :: RoomMeta -> RoomMeta
updateRoomName rd = rd { display_name = roomNameFromData rd }

foldEventIntoRoomState :: MatrixEvent MatrixRoomEvent -> RoomMeta -> RoomMeta
foldEventIntoRoomState { content } st = case content of
  Right (RoomName name) -> updateRoomName $ st { name = Just name }
  Right (RoomTopic topic) -> updateRoomName $ st { topic = Just topic }
  Right (RoomCanonicalAlias cs) -> updateRoomName $ st { canonical_alias = Just cs }
  Right (Membership m) -> updateRoomName $ st { members = Map.insert m.user_id m.profile st.members }
  _ -> st

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
    }

-- A bundle of Dynamics related to a given room.
type JoinedRoom m
  = { messages :: Dynamic Int -> m (Dynamic (Array (MatrixEvent MatrixRoomEvent)))
    , meta :: Dynamic RoomMeta
    }

-- dynState :: forall m. MonadFRP m => m (Dynamic Map RoomId (JoinedRoom m))
type KnownServerState m
  = { joined_rooms :: Map RoomId (JoinedRoom m)
    , invited_to :: Set RoomId
    }

type RoomLeave
  = {}

type RoomInvite
  = {}

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

-- Decode a RoomUpdate from JSON.
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

-- Perform a single long-poll request to the /sync endpoint.
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

-- Kick off an async process that periodically calls the callabck whenever new information from the server is available.
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

delayTillFirstUpdate :: forall a m. MonadFRP m => Dynamic a -> m (WeakDynamic a)
delayTillFirstUpdate d = do
  holdWeakDyn $ changed d

handleJoin :: forall m. MonadFRP m => SessionInfo -> Tuple RoomId RoomUpdate -> Event SyncPollResult -> InnerFRP (JoinedRoom m)
handleJoin si (Tuple rId ru) updates = do
  -- { dynamic: msg_dyn, read: msg_read, set: msg_set } <- newDynamic []
  let
    init_meta =
      { room_id: rId
      , canonical_alias: Nothing
      , topic: Nothing
      , members: Map.empty
      , display_name: unRoomId rId
      , name: Nothing
      }

    rus :: Event RoomUpdate
    rus = filterMapEvent (\spr -> Map.lookup rId spr.rooms.join) updates
  messages :: Dynamic (Array (MatrixEvent MatrixRoomEvent)) <-
    foldDyn (\ru' a -> Array.slice 0 100 $ a <> ru'.new_events) [] rus
  meta :: Dynamic RoomMeta <-
    foldDyn (\ru' m -> Array.foldl (flip foldEventIntoRoomState) m ru'.new_events) init_meta rus
  pure
    { messages: const $ pure messages
    , meta
    }

-- -- A Dynamic representing the currently known user-relevant state of the Matrix server
-- -- This data need not be entirely complete, it is simply what is known at the current
-- -- time, and of that, only the parts that we currently need.
serverState :: forall m. MonadFRP m => SessionInfo -> m (WeakDynamic (KnownServerState m))
serverState si = do
  updates <- (syncFeed si) :: m (Event SyncPollResult)
  let 
    foldStepTuple :: Map RoomId (JoinedRoom m) -> Tuple RoomId RoomUpdate -> InnerFRP (Map RoomId (JoinedRoom m))
    foldStepTuple st (Tuple rId ru) = case (Map.lookup rId st) of
      Just _ -> pure st
      Nothing -> do
        jr <- (handleJoin si (Tuple rId ru) updates) :: InnerFRP (JoinedRoom m)
        pure $ Map.insert rId jr st
    foldStep :: SyncPollResult -> Map RoomId (JoinedRoom m) -> InnerFRP (Map RoomId (JoinedRoom m))
    foldStep spr st = do
      let tuples = Map.toUnfoldable $ Map.difference spr.rooms.join st
      Array.foldM foldStepTuple Map.empty tuples
  joined_rooms <- (foldDynM foldStep Map.empty updates) :: m _
  invited_to <-
    (foldDyn
      ( \updt st ->
          Set.difference (Set.union st (Map.keys updt.rooms.invite)) (Map.keys updt.rooms.join)
      )
      Set.empty
      updates) :: m _

  delayTillFirstUpdate
    $ do
        j <- joined_rooms
        i <- invited_to
        pure { joined_rooms: j, invited_to: i }
 -- delayTillFirstUpdate --   =<< foldDynEffect --       ( \srp kss -> do --           new_joins <- traverse $ handleJoin <$> Map.toUnfoldable $ Map.difference srp.rooms.join kss.joined_rooms --           pure kss --       ) --       { invited_to: (Set.empty :: Set RoomId), joined_rooms: Map.empty } --       updates -- holdDyn $ sampleAt ?wut (current $ unWeakDynamic rooms) --   { dynamic: st_dyn, read: st_read, set: st_set } <- --     liftEffect $ newDynamic ({ st_rooms: Map.empty, st_invites: Set.empty } :: InternalState m) --   subscribeEvent_ --     ( \(spr :: SyncPollResult) -> do --         st <- st_read --         st' <- foldSrpStep si spr st --         st_set st' --     ) --     updates --   let --     extractOutput :: InternalState m -> KnownServerState m --     extractOutput st = --       { invited_to: st.st_invites --       , joined_rooms: (map (_.dynamic) st.st_rooms) --       } --   holdWeakDyn $ changed (st_dyn <#> extractOutput)