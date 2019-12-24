module RoomWidget (roomView, joinedRoomView) where

import Prelude

import API.Core (sendMessage)
import API.Rooms (leaveRoom)
import Control.Apply (lift2)
import Control.Monad.Cleanup (onCleanup)
import CustomCombinators (affButtonLoopSimplified, childListMutations, dynamicHitsTarget, dynamicMaybe_, elClass, elClass', gateEventBy, pulseSpinner, sampleFn, subscribeEvent, withPast)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Timer (clearTimeout, setTimeout)
import Foreign.Object as Object
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.JoinRoomWidget (joinRoomView)
import Purechat.ServerFeed (RoomMeta, JoinedRoom)
import Purechat.Types (MatrixEvent, MatrixRoomEvent(..), RoomId, RoomMembership(..), SessionInfo, UserProfile, unUserId)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (domEventWithSample, el', elAttr, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (class MonadFRP, Dynamic, Event, current, dynamic_, filterMapEvent, fixFRP, fixFRP_, foldDyn, holdDyn, subscribeEvent_, tagDyn)
import Specular.FRP.Async (asyncRequestMaybe)
import Specular.FRP.List (dynamicList_)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (scrollHeight, scrollTop, setScrollTop)
import Web.HTML.HTMLElement (offsetHeight)

textareaOnChangeWithReset :: forall m. MonadWidget m => Event Unit -> m (Dynamic String)
textareaOnChangeWithReset reset = do
  Tuple node _ <- el' "textarea" (pure unit)
  changed <- domEventWithSample (\_ -> getTextInputValue node) "change" node
  subscribeEvent_ (\_ -> setTextInputValue node "") reset
  holdDyn "" changed

composeMessageWidget :: forall m. MonadWidget m => m (Event String)
composeMessageWidget =
  let
    loop :: Event Unit -> m (Tuple (Event Unit) (Event String))
    loop reset = do
      composeMessage :: Dynamic String <- textareaOnChangeWithReset reset
      sendBtnClicked :: Event Unit <- buttonOnClick (pure mempty) (text "Send")
      let
        outbox = tagDyn composeMessage sendBtnClicked

        resetOut = const unit <$> outbox
      pure $ Tuple resetOut outbox
  in
    elClass "div" "message-form" $ fixFRP loop

viewEvent :: forall m. MonadWidget m => SessionInfo -> Dynamic RoomMeta -> MatrixEvent MatrixRoomEvent -> m Unit
viewEvent si drd evt =
  elClass "div" "message-wrapper" $ dynamic_ $ drd
    <#> \rd -> do
        let
          sender_profile :: Maybe UserProfile
          sender_profile = Map.lookup evt.sender rd.members

          sender_displayname = case sender_profile of
            Just { displayname: (Just n) } -> n
            _ -> unUserId evt.sender
        showAvatarOrDefault si (sender_profile >>= (\p -> p.avatar_url))
        elClass "div" "msg"
          $ case evt.content of
              Left errMsg -> elClass "p" "error" $ text $ "Error while decoding message from " <> sender_displayname <> ": " <> errMsg
              Right (Message { body }) -> do
                elClass "p" "username" $ text sender_displayname
                elClass "p" "message" $ text body
              Right (Membership { profile, membership: Join }) -> text $ sender_displayname <> " joined the room."
              Right (Membership { profile, membership: Invite }) -> text $ sender_displayname <> " has been invited to the room."
              Right (Membership { profile, membership: Leave }) -> text $ sender_displayname <> " left room."
              Right (RoomName n) -> text $ sender_displayname <> " set the room's name to " <> n
              Right (RoomTopic n) -> text $ sender_displayname <> " set the room's topic to " <> n
              Right (RoomCanonicalAlias n) -> text $ sender_displayname <> " set the room's canonical alias to " <> n

leaveButton :: forall m. MonadWidget m => SessionInfo -> RoomId -> m Unit
leaveButton si rId = do
  _ <-
    affButtonLoopSimplified
      { ready:
        \_ -> do
          leave <- buttonOnClick (pure Object.empty) $ elClass "i" "fas fa-sign-out-alt" (pure unit)
          pure $ (const $ leaveRoom si rId) <$> leave
      , loading:
        do
          pulseSpinner
          text "Leaving..."
      , success: const $ pure unit
      }
  pure unit

scrollTopDyn :: forall m. MonadFRP m => Node -> m (Dynamic Number)
scrollTopDyn n = do
  initialScroll <- liftEffect $ scrollTop (unsafeCoerce n)
  scrolls <- domEventWithSample (\evt -> scrollTop (unsafeCoerce n)) "scroll" n
  holdDyn initialScroll scrolls

-- | The bar at the top of the room view with the room's display name and "leave" button.
roomTopBar :: forall m. MonadWidget m => SessionInfo -> RoomId -> Dynamic RoomMeta -> m Unit
roomTopBar si rId meta = do
  -- Show the room name. Possibly in the future, add 
  -- possibility of clicking to enable typing in a room ID
  elClass "div" "room-meta" do
    elClass "h2" "room-name" $ dynamic_ $ meta <#> \rs -> (text rs.display_name)
    leaveButton si rId
  -- A simple horizontal line separating the meta-area from the content.
  elClass "hr" "roomname-content-set" (pure unit)

-- | Implements the auto-scrolling logic that follows
-- | the addition of messages both at the top and the bottom of the message view.
autoScroll :: forall m. MonadFRP m => Node -> Dynamic (Array (MatrixEvent MatrixRoomEvent)) -> m Unit
autoScroll msgListNode msgs = do

  listMutations <- childListMutations msgListNode { childList: true }
  listHeightUpdates <- subscribeEvent (\_ -> scrollHeight (unsafeCoerce msgListNode)) listMutations
  let
    listSnapshots =
      sampleFn
        ( \h msg ->
            { listHeight: h
            , fstId: Array.head msg <#> _.event_id
            , lstId: Array.last msg <#> _.event_id
            }
        )
        listHeightUpdates
        (current msgs)

    scrollToBottom :: Effect Unit
    scrollToBottom = do
      h <- scrollHeight $ unsafeCoerce msgListNode
      setScrollTop h (unsafeCoerce msgListNode)

  snapshotPairs <- withPast (lift2 Tuple) Nothing $ Just <$> listSnapshots
  flip subscribeEvent_ (filterMapEvent identity snapshotPairs)
    $ \(Tuple pst new) -> do
        scrlAt <- scrollTop (unsafeCoerce msgListNode)
        listVisibleHeight <- offsetHeight (unsafeCoerce msgListNode)
        when (not (pst.lstId == new.lstId) && scrlAt + listVisibleHeight == pst.listHeight) do
          scrollToBottom
        when (not $ pst.fstId == new.fstId) do
          setScrollTop (scrlAt - pst.listHeight + new.listHeight) (unsafeCoerce msgListNode)
  to <- liftEffect $ setTimeout 100 scrollToBottom
  onCleanup $ clearTimeout to

-- | Component that displays a list of messages, allowing for lazy loading and updates.
messageListView :: forall m. MonadWidget m => SessionInfo -> RoomId -> JoinedRoom m -> m Unit
messageListView si rId room =
  fixFRP_ \requestMore -> do
    let
      infscrollStep = 20
      init_load = 25

    demand <- foldDyn (\a b -> b + infscrollStep) init_load requestMore
    subscribeEvent_ (\_ -> Console.log "Requesting more!") requestMore
    msgs <- room.messages $ demand
    -- List of room events. Keep reference to the node since we need it for some scrolling magic.
    (Tuple msgListNode _) <-
      elClass' "div" "room-messages"
        $ dynamicList_ msgs
        $ \devt ->
            dynamic_ $ devt <#> (viewEvent si room.meta)
    -- If the user is at the bottom of the page and it expands, take the user to the new bottom.
    autoScroll msgListNode msgs
    -- Keep track of how far the user has scrolled.
    scrl <- scrollTopDyn msgListNode
    -- Event that fires whenevr the user has hit the top of the page.
    scrollHitZero <- dynamicHitsTarget scrl 0.0
    -- Requesting more logic.
    pure $ gateEventBy scrollHitZero (not <$> current room.loadingMessages)

-- A widget showing the contents of a room that the user is currently participating in.
joinedRoomView :: forall m. MonadWidget m => SessionInfo -> RoomId -> JoinedRoom m -> m Unit
joinedRoomView si rId room = do
  liftEffect $ Console.log "Room."
  roomTopBar si rId room.meta
  -- Display a loading spinner at the top of the page whenever new messages are being loaded.
  dynamic_ $ room.loadingMessages <#> \l -> when l pulseSpinner
  messageListView si rId room
  -- Message composition widget, which will produce message events whenever "send" is clicked.
  msg <- composeMessageWidget
  currentRequest <- holdDyn Nothing (map Just (sendMessage si rId <$> msg))
  result <- asyncRequestMaybe currentRequest
  pure unit

-- A single "room view". Think of this as a browser tab with an address bar that can show any
-- accessible room in the Matrix federation. Note that this widget is applicable regardless of
-- join status. If the user is not in the room, they will be shown join/invite options instead.
-- The room directory separate from the room view. Think of the directory as a "remote control"
-- for this widget/ The room view widget can function independently from the directory.
roomView :: forall m. MonadWidget m => SessionInfo -> RoomId -> Dynamic (Maybe (JoinedRoom m)) -> m Unit
roomView si rId mrd =
  elAttr "div" (Object.fromFoldable [ Tuple "class" "room-view" ])
    $ do
        dynamicMaybe_ mrd (joinedRoomView si rId)
        dynamic_ $ mrd
          <#> case _ of
              Just _ -> pure unit
              Nothing -> joinRoomView si rId
