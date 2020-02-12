module RoomWidget (roomView, joinedRoomView) where

import Prelude

import API.Core (sendTextMessage)
import API.Rooms (RoomMeta, leaveRoom)
import Control.Apply (lift2, lift3)
import CustomCombinators (affButtonLoopSimplified, childListMutations, clockMilliseconds, dynamicHitsTarget, elClass, elClass', elemOnClick, filterByBool, gateEventBy, holdPast, pulseSpinner, sampleFn, subscribeEvent, withPastSkipFirst)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.Event (MatrixEvent(..), MatrixRoomEvent(..), MessageType(..))
import Purechat.GlobalEnv (GlobalEnv)
import Purechat.JoinRoomWidget (joinRoomView)
import Purechat.ServerFeed (JoinedRoom)
import Purechat.Types (RoomId, RoomMembership(..), SessionInfo, UserProfile, unUserId)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (domEvent, domEventWithSample, el', text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (class MonadFRP, Dynamic, Event, changed, current, dynamic, dynamic_, filterJustEvent, fixFRP, fixFRP_, foldDyn, holdDyn, leftmost, never, subscribeDyn_, subscribeEvent_, switch, tagDyn)
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

composeMessageWidget :: forall m. MonadWidget m => GlobalEnv m -> m (Event String)
composeMessageWidget env =
  let
    loop :: Event Unit -> m (Tuple (Event Unit) (Event String))
    loop reset = do
      composeMessage :: Dynamic String <- textareaOnChangeWithReset reset

    --   av_url_updates <- affButtonLoopSimplified
    -- $ case _ of
    --     NotRequested -> do
    --       fileChosen <- fileInputOnChange
    --       pure $ (uploadMXC session) <<< toBlob <$> filterMapEvent identity fileChosen
    --     Loading -> do
    --       pulseSpinner
    --       pure never
    --     Loaded (Left err) -> do
    --       fileChosen <- fileInputOnChange
    --       pure $ (uploadMXC session) <<< toBlob <$> filterMapEvent identity fileChosen
    --     Loaded (Right mxc) -> do
    --       fileChosen <- fileInputOnChange
    --       pure $ (uploadMXC session) <<< toBlob <$> filterMapEvent identity fileChosen

  -- holdDyn Nothing (map Just av_url_updates)
      
      -- attachmentUrl <- invisibleUploadingFileInput attachButton =<< do
      --   buttonOnClick (pure mempty) $ do
      --     elClass "i" "fas fa-paperclip" $ pure unit

      sendBtnClicked :: Event Unit <- buttonOnClick (pure mempty) $ do
        elClass "i" "fas fa-paper-plane" $ pure unit
      let
        outbox = tagDyn composeMessage sendBtnClicked

        resetOut = const unit <$> outbox
      pure $ Tuple resetOut outbox
  in
    elClass "div" "message-form" $ fixFRP loop

viewEvent :: forall m. MonadWidget m => GlobalEnv m -> Dynamic RoomMeta -> MatrixEvent MatrixRoomEvent -> m Unit
viewEvent env drd (MatrixEvent evt) =
  elClass "div" "message-wrapper" $ dynamic_ $ drd <#> \rd -> do
    let
      sender_profile :: Maybe UserProfile
      sender_profile = Map.lookup evt.sender rd.members

      sender_displayname = case sender_profile of
        Just { displayname: (Just n) } -> n
        _ -> unUserId evt.sender

      clickeableUsername :: m Unit
      clickeableUsername = do
        (Tuple usernameNode _ ) <- elClass' "p" "username" $ text $ sender_displayname
        subscribeEvent_ (\_ -> env.showProfile evt.sender sender_profile) =<< domEvent "click" usernameNode

      messageView :: String -> m Unit
      messageView body = do
        elClass "div" "event-message" do
          clickeableUsername
          elClass "p" "message-body" $ text body
      
      actionView :: String -> m Unit
      actionView action = do
        elClass "div" "event-action" do
          clickeableUsername
          text action

    elClass "div" "avatar-wrapper" $ 
      showAvatarOrDefault env.session (sender_profile >>= (\p -> p.avatar_url))

    case evt.content of
      Left errMsg -> elClass "p" "error" $ text $ "Error while decoding message from " <> sender_displayname <> ": " <> errMsg
      Right (Message { body, msgtype }) -> case msgtype of
        Emote -> elClass "div" "event-emote" do
          clickeableUsername
          elClass "p" "message-body" $ text body
        Notice -> elClass "div" "event-notice" do
          clickeableUsername
          elClass "p" "message-body" $ text body
        -- Image url -> 
        -- File URL
        -- Audio URL
        -- Location String
        -- Video URL
        -- Unknown String
        _ -> messageView body
      Right (Membership { profile, membership: Join }) -> actionView " joined the room."
      Right (Membership { profile, membership: Invite }) -> actionView " has been invited to the room."
      Right (Membership { profile, membership: Leave }) -> actionView " the left room."
      Right (RoomAvatar av) -> actionView $ " changed the room's avatar."
      Right (RoomName n) -> actionView $ " set the room's name to " <> n
      Right (RoomTopic n) -> actionView $ " set the room's topic to " <> n
      Right (RoomCanonicalAlias n) -> actionView $ " set the room's canonical alias to " <> n
      Right (RoomPinnedEvents pins) -> actionView " changed the pinned messages."
      Right (UnknownRoomEvent evtt) -> actionView $ " unknown event type " <> evtt

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
    dynamic_ $ meta <#> \rs -> do
      showAvatarOrDefault si rs.avatar_url
      elClass "h2" "room-name" $  (text rs.display_name)
    leaveButton si rId
  -- A simple horizontal line separating the meta-area from the content.
  elClass "hr" "roomname-content-set" (pure unit)

data Triple a b c = Triple a b c

-- | Implements the auto-scrolling logic that follows
-- | the addition of messages both at the top and the bottom of the message view.
autoScroll :: forall m. MonadFRP m => Node -> Event Unit -> Dynamic (Array (MatrixEvent MatrixRoomEvent)) -> m { following :: Dynamic Boolean }
autoScroll msgListNode reset msgs = do

  listMutations <- childListMutations msgListNode { childList: true }
  
  sizePollClock <- clockMilliseconds 500

  listHeight :: Dynamic Number <- do
    sH <- liftEffect $ scrollHeight (unsafeCoerce msgListNode)
    holdDyn sH =<<  subscribeEvent (\_ -> scrollHeight (unsafeCoerce msgListNode)) (leftmost [listMutations <#> const unit, sizePollClock <#> const unit])
  
  listVisibleHeight :: Dynamic Number <- do
    -- TODO Maybe listen to resize events?
    oH <- liftEffect $ offsetHeight (unsafeCoerce msgListNode)
    
    holdDyn oH =<< subscribeEvent (\_ -> offsetHeight (unsafeCoerce msgListNode)) (leftmost [listMutations <#> const unit, sizePollClock <#> const unit])
  
  scrl :: Dynamic Number <- scrollTopDyn msgListNode
    
  let
    listSnapshot = lift2 ( \h msg ->
            { listHeight: h
            , fstId: Array.head msg <#> (unwrap >>> _.event_id)
            , lstId: Array.last msg <#> (unwrap >>> _.event_id)
            }
        ) listHeight msgs

    isAtBottom = lift3 (\s h lh -> s + h >= lh) scrl listVisibleHeight listHeight
  
  subscribeDyn_ (unsafeCoerce >>> Console.log) $ lift3 Triple scrl listVisibleHeight listHeight

  wasAtBottom :: Dynamic Boolean <- map (fromMaybe false) <$> holdPast (changed isAtBottom)

  lastChanged <- filterByBool <$> withPastSkipFirst (\pst new -> pst.lstId /= new.lstId) (changed listSnapshot)
  let 
    scrollToBottom :: Event Unit 
    scrollToBottom = gateEventBy lastChanged $ current wasAtBottom

  firstChanged <- filterJustEvent <$> withPastSkipFirst (\pst new -> if pst.fstId /= new.fstId then (Just (new.listHeight - pst.listHeight)) else Nothing) (changed listSnapshot)
  let scrollKeepRelative = sampleFn (+) firstChanged (current scrl)

  subscribeEvent_ (flip setScrollTop (unsafeCoerce msgListNode)) $ leftmost [tagDyn listHeight reset, scrollToBottom <#> const 0.0, scrollKeepRelative]

  pure {following : isAtBottom}

-- | Component that displays a list of messages, allowing for lazy loading and updates.
messageListView :: forall m. MonadWidget m => GlobalEnv m -> RoomId -> JoinedRoom m -> m Unit
messageListView env rId room =
  fixFRP_ \requestMore -> do
    let
      infscrollStep = 20
      init_load = 25

    demand <- foldDyn (\a b -> b + infscrollStep) init_load requestMore
    msgs <- room.messages $ demand
    -- List of room events. Keep reference to the node since we need it for some scrolling magic.
    (Tuple msgListNode _) <-
      elClass' "div" "room-messages"
        $ dynamicList_ msgs
        $ \devt ->
            dynamic_ $ devt <#> (viewEvent env room.meta)
    -- If the user is at the bottom of the page and it expands, take the user to the new bottom.
    
    fixFRP_ \reset -> do
      {following} <- autoScroll msgListNode reset msgs
      map switch <$> dynamic $ following <#> \f -> case f of
        false -> elemOnClick "div" (Object.singleton "class" "not-following-warning-wrapper") $ elClass "div" "not-following-warning"  $ text "Reading earlier messages. Click to return to present."
        true -> pure never

    -- Keep track of how far the user has scrolled.
    scrl <- scrollTopDyn msgListNode
    -- Event that fires whenevr the user has hit the top of the page.
    scrollHitZero <- dynamicHitsTarget scrl 0.0
    -- Requesting more logic.
    pure $ gateEventBy scrollHitZero (not <$> current room.loadingMessages)

-- A widget showing the contents of a room that the user is currently participating in.
joinedRoomView :: forall m. MonadWidget m => GlobalEnv m -> RoomId -> JoinedRoom m -> m Unit
joinedRoomView env rId room = do

  roomTopBar env.session rId room.meta

  -- Display a loading spinner at the top of the page whenever new messages are being loaded.
  dynamic_ $ room.loadingMessages <#> \l -> when l pulseSpinner

  messageListView env rId room

  -- Message composition widget, which will produce message events whenever "send" is clicked.
  fixFRP_ \messages -> do
    currentRequest <- holdDyn Nothing (map Just (sendTextMessage env.session rId <$> messages))
    result <- asyncRequestMaybe currentRequest
    composeMessageWidget env

  pure unit

-- A single "room view". Think of this as a browser tab with an address bar that can show any
-- accessible room in the Matrix federation. Note that this widget is applicable regardless of
-- join status. If the user is not in the room, they will be shown join/invite options instead.
-- The room directory separate from the room view. Think of the directory as a "remote control"
-- for this widget/ The room view widget can function independently from the directory.
roomView :: forall m. MonadWidget m => GlobalEnv m -> RoomId -> Dynamic (Maybe (JoinedRoom m)) -> m Unit
roomView env rId mrd =
  elClass "div" "room-view" $ dynamic_ $ mrd <#> case _ of
    Just jr -> joinedRoomView env rId jr
    Nothing -> joinRoomView env.session rId
