module Purechat.ServerFeed (serverState, KnownServerState, JoinedRoom, RoomMeta) where

import Prelude
import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Cleanup (onCleanup)
import CustomCombinators (RemoteResourceView, fanOutM, toLoadedUpdates)
import Data.Argonaut (Json, decodeJson, getField, (.:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, findMax)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as EE
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign.Object (Object)
import Purechat.Types (LoginToken(..), MatrixEvent, MatrixRoomEvent(..), PrevBatchToken(..), RoomId(..), SessionInfo, UserId, UserProfile, decodeRoomEvent, unRoomId)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, changed, foldDyn, holdWeakDyn, newDynamic, newEvent, subscribeDyn_)
import Specular.FRP.Async (startAff)
import Unsafe.Coerce (unsafeCoerce)

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
  = { joined_rooms :: Dynamic (Map RoomId (JoinedRoom m))
    , invited_to :: Dynamic (Set RoomId)
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

type InternalRoomState m
  = { addUpdate :: RoomUpdate -> Effect Unit
    , output :: JoinedRoom m
    , cleanup :: Effect Unit
    }

initMeta :: RoomId -> RoomMeta
initMeta rId =
  { room_id: rId
  , canonical_alias: Nothing
  , topic: Nothing
  , members: Map.empty
  , display_name: unRoomId rId
  , name: Nothing
  }

succMaxKey :: forall v. Map Int v -> Int
succMaxKey m = fromMaybe 0 $ findMax m <#> (\entry -> entry.key + 1)

-- foldStepTuple :: forall m. MonadFRP m =>
--   Map RoomId (InternalRoomState m) ->
--   Tuple RoomId RoomUpdate ->
--   CleanupT Effect (Map RoomId (InternalRoomState m))
-- foldStepTuple st' (Tuple rId ru) = case (Map.lookup rId st') of
--   Just (rst :: InternalRoomState m) -> do
--     liftEffect $ rst.addUpdate ru
--     pure st'
--   Nothing -> do
-- A Dynamic representing the currently known user-relevant state of the Matrix server
-- This data need not be entirely complete, it is simply what is known at the current
-- time, and of that, only the parts that we currently need.
serverState :: forall m. MonadFRP m => SessionInfo -> m (Dynamic (RemoteResourceView (KnownServerState m)))
serverState si = do
  feed :: Event SyncPollResult <- syncFeed si
  joined_rooms <-
    fanOutM (feed <#> \ru -> (Map.toUnfoldable $ ru.rooms.join) :: Array _)
      $ \rId ru rus -> do
          let
            foldUpdate ruu meta = Array.foldl (flip foldEventIntoRoomState) meta ruu.new_events
          -- Intialize the room metadata by folding the initial set of events, and keeping an Effect to update it.
          nd_meta <- foldDyn foldUpdate (foldUpdate ru $ initMeta rId) rus
          -- Initialize a map of (Dynamic Int) that indicate how many messages each view on the room wants to have loaded.
          -- The key is used to delete entries on cleannup.
          nd_demands <- newDynamic Map.empty
          let
            total_demand = do
              dems <- nd_demands.dynamic
              foldM (\m dem -> dem >>= \d -> pure (max d m)) 0 dems
          subscribeDyn_ (\d -> Console.log (unsafeCoerce d)) total_demand
          -- A dynamic that accumlates messages (simply room events for now)
          -- Features lazy loading: see `nd_demands`
          nd_messages <- newDynamic $ ru.new_events
          pure
            { messages:
              \demand -> do
                -- Add a new "demand" dynamic to the set of demand dynamics.
                demands <- liftEffect $ nd_demands.read
                let
                  cleanupKey = succMaxKey demands
                liftEffect $ nd_demands.set (Map.insert cleanupKey demand demands)
                -- Make sure to delete our demand once it's no longer needed to avoid retaining too many messages
                onCleanup $ nd_demands.read >>= (nd_demands.set <<< Map.delete cleanupKey)
                -- Return the list of messages, it'll eventually update to fit the demand.
                pure nd_messages.dynamic
            , meta: nd_meta
            }
  joined_rooms_rr :: Dynamic (RemoteResourceView (Map RoomId (JoinedRoom m))) <-
    toLoadedUpdates $ changed joined_rooms
  pure
    $ map
        ( map
            $ const
                { joined_rooms: joined_rooms
                , invited_to: pure $ Set.empty
                }
        )
        joined_rooms_rr
 -- rooms_st <- newDynamic (Map.empty :: Map RoomId (InternalRoomState m))  --   -- cleanups :: Ref.Ref (Effect Unit) <- liftEffect $ Ref.new (pure unit)  -- onCleanup $ Ref.read cleanups >>= \c -> c  -- flip subscribeEvent_ updates \spr -> do  --   st <- rooms_st.read  --   (Tuple st' cleanup) <- runCleanupT $ Array.foldM foldStepTuple st (Map.toUnfoldable spr.rooms.join)  --   _ <- Ref.modify (\c -> c >>= const cleanup) cleanups  --   rooms_st.set st'  -- joined_rooms <- toLoadedUpdates $ changed $ rooms_st.dynamic 