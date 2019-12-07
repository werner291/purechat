module Purechat.ServerFeed (serverState, KnownServerState, JoinedRoom, RoomMeta) where

import Prelude
import API.Rooms (getEventsUpto)
import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Control.Apply (lift2)
import Control.Monad.Cleanup (CleanupT, onCleanup)
import CustomCombinators (RemoteResourceView, affSuccesses, fanOutM, toLoadedUpdates)
import Data.Argonaut (Json, decodeJson, getField, (.:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List as List
import Data.Map (Map, findMax)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as EE
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Purechat.Types (LoginToken(..), MatrixEvent, MatrixRoomEvent(..), PrevBatchToken(..), RoomId(..), SessionInfo, TimeEventId, UserId, UserProfile, decodeRoomEvent, getTimeId, unRoomId)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, changed, fixFRP, foldDyn, holdDyn, holdWeakDyn, mergeEvents, newDynamic, newEvent)
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe, startAff)

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
    , loadingMessages :: Dynamic Boolean
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
  = { new_timeline_events :: Array (MatrixEvent MatrixRoomEvent)
    , new_state_events :: Array (MatrixEvent MatrixRoomEvent)
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
    { new_timeline_events
    , new_state_events
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

foldUpdateIntoRoomMeta :: RoomUpdate -> RoomMeta -> RoomMeta
foldUpdateIntoRoomMeta ruu meta = Array.foldl (flip foldEventIntoRoomState) meta (ruu.new_state_events <> ruu.new_timeline_events)

tupleizeFeed :: Event SyncPollResult -> Event (Array (Tuple RoomId RoomUpdate))
tupleizeFeed feed = feed <#> \ru -> (Map.toUnfoldable $ ru.rooms.join)

combineDemands :: Dynamic (Map Int (Dynamic Int)) -> Dynamic Int
combineDemands dems = foldM (\m dem -> dem >>= \d -> pure (max d m)) 0 =<< dems

timestampify :: forall e. Array (MatrixEvent e) -> Map TimeEventId (MatrixEvent e)
timestampify evts = Map.fromFoldable $ evts <#> \ev -> Tuple (getTimeId ev) ev

type EventsPage
  = { start :: Maybe PrevBatchToken
    , events :: Array (MatrixEvent MatrixRoomEvent)
    }

-- | Run the loop that merges numbers 
loadMessageLoop ::
  forall m.
  MonadFRP m =>
  SessionInfo ->
  RoomId ->
  Dynamic Int ->
  EventsPage ->
  Event (Array (MatrixEvent MatrixRoomEvent)) ->
  m
    { messages :: Dynamic (Array (MatrixEvent MatrixRoomEvent))
    , loading :: Dynamic Boolean
    }
loadMessageLoop si rId demand init_page updates =
  fixFRP
    $ \new_pages -> do
        let
          pages_events :: Event (Map TimeEventId (MatrixEvent MatrixRoomEvent))
          pages_events = new_pages <#> _.events >>> timestampify

          all_new_events :: Event (Map TimeEventId (MatrixEvent MatrixRoomEvent))
          all_new_events = mergeEvents pure pure (\l r -> pure (l <> r)) (timestampify <$> updates) pages_events
        nd_messages :: Dynamic (Map TimeEventId (MatrixEvent MatrixRoomEvent)) <-
          foldDyn (<>) (timestampify init_page.events) all_new_events
        -- Keeps track of which token to load more messages from.
        -- Becomes Nothing if no more messages are available.
        load_from_token :: Dynamic (Maybe PrevBatchToken) <-
          holdDyn init_page.start (new_pages <#> _.start)
        let
          -- Turn the map into a sorted array.
          linearized = map (List.toUnfoldable <<< Map.values) nd_messages

          -- Compute how many messages we still need to load.
          deficit = lift2 (\msgs dem -> dem - Array.length msgs) linearized demand
        latestPageRequest <-
          asyncRequestMaybe do
            d <- deficit
            b <- load_from_token
            case b of
              Just tk
                | d > 0 ->
                  pure $ Just
                    $ ( getEventsUpto si rId tk
                          <#> \res ->
                              { events: res.chunk
                              , start: if Array.null res.chunk then Nothing else res.from
                              }
                      )
              _ -> pure Nothing
        let
          new_pages' = affSuccesses latestPageRequest
        pure
          $ Tuple new_pages'
              { messages: linearized
              , loading:
                latestPageRequest
                  <#> ( case _ of
                        Loading -> true
                        _ -> false
                    )
              }

-- Initialize a room feed from an initial room update.
mkRoomFeed ::
  forall m.
  MonadFRP m =>
  SessionInfo ->
  RoomId ->
  RoomUpdate -> 
  Event (RoomUpdate) -> 
  CleanupT Effect (JoinedRoom m)
mkRoomFeed si rId ru rus = do
  let
    initMetaWithFirstEvents = foldUpdateIntoRoomMeta ru $ initMeta rId
  -- Intialize the room metadata by folding the initial set of events, and keeping an Effect to update it.
  meta :: Dynamic RoomMeta <- foldDyn foldUpdateIntoRoomMeta initMetaWithFirstEvents rus
  -- Initialize a map of (Dynamic Int) that indicate how many messages each view on the room wants to have loaded.
  -- The key is used to delete entries on cleannup.
  nd_demands <- newDynamic Map.empty
  { messages: nd_messages, loading: loadingMessages } <- loadMessageLoop si rId (combineDemands nd_demands.dynamic) { start: (Just ru.prev_batch), events: ru.new_timeline_events } (rus <#> _.new_timeline_events)
  let
    messages :: Dynamic Int -> m (Dynamic (Array (MatrixEvent MatrixRoomEvent)))
    messages demand = do
      -- Add a new "demand" dynamic to the set of demand dynamics.
      demands <- liftEffect $ nd_demands.read
      let
        cleanupKey = succMaxKey demands
      liftEffect $ nd_demands.set (Map.insert cleanupKey demand demands)
      -- Make sure to delete our demand once it's no longer needed to avoid retaining too many messages
      onCleanup $ nd_demands.read >>= (nd_demands.set <<< Map.delete cleanupKey)
      -- Return the list of messages, it'll eventually update to fit the demand.
      pure nd_messages
  pure { messages, loadingMessages, meta }

serverState :: forall m. MonadFRP m => SessionInfo -> m (Dynamic (RemoteResourceView (KnownServerState m)))
serverState si = do
  feed :: Event SyncPollResult <- syncFeed si
  joined_rooms <- fanOutM (tupleizeFeed feed) (mkRoomFeed si)
  joined_rooms_rr :: Dynamic (RemoteResourceView (Map RoomId (JoinedRoom m))) <-
    toLoadedUpdates $ changed joined_rooms
  pure $ map (map $ const { joined_rooms: joined_rooms, invited_to: pure $ Set.empty }) joined_rooms_rr
 -- rooms_st <- newDynamic (Map.empty :: Map RoomId (InternalRoomState m))   --    -- cleanups :: Ref.Ref (Effect Unit) <- liftEffect $ Ref.new (pure unit)   -- onCleanup $ Ref.read cleanups >>= \c -> c   -- flip subscribeEvent_ updates \spr -> do  --   st <- rooms_st.read  --   (Tuple st' cleanup) <- runCleanupT $ Array.foldM foldStepTuple st (Map.toUnfoldable spr.rooms.join)  --   _ <- Ref.modify (\c -> c >>= const cleanup) cleanups  --   rooms_st.set st'  -- joined_rooms <- toLoadedUpdates $ changed $ rooms_st.dynamic 