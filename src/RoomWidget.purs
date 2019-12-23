module RoomWidget (roomView, joinedRoomView) where

import Prelude

import API.Core (sendMessage)
import API.Rooms (leaveRoom)
import CustomCombinators (affButtonLoopSimplified, clockMilliseconds, dynamicMaybe_, elClass, elClass', holdPast, pulseSpinner, subscribeEvent, withPast)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Timer (setTimeout)
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
import Specular.FRP (class MonadFRP, Dynamic, Event, changed, current, dynamic_, filterEvent, filterMapEvent, fixFRP, foldDyn, holdDyn, readDynamic, sampleAt, subscribeEvent_, tagDyn)
import Specular.FRP.Async (asyncRequestMaybe)
import Specular.FRP.List (dynamicList_)
import Specular.Ref (newRef)
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

-- autoScrollBar :: forall m. MonadWidget m => Node -> Event Unit -> m Unit
-- autoScrollBar msgListNode trigger = do
--   -- A horizontal strip of space reserved for the checkbox that causes the view to auto-scroll down.
--   elClass "div" "auto-scoll-bar" do
--     autoScroll <- checkbox true Object.empty
--     text "Auto-scroll"
--     let
--       elt = unsafeCoerce msgListNode

--       scrollToBottomCond = do
--         h <- scrollHeight elt
--         setScrollTop h elt
--     liftEffect do
--       autoScrl <- pull $ readBehavior (current autoScroll)
--       when autoScrl scrollToBottomCond
--     subscribeEvent_ (const scrollToBottomCond) trigger

-- Dynamic that follows the `scrollTop` property of a node, 
-- which is a number indicating how far a user has scrolled down the page.
scrollTopDyn :: forall m. MonadFRP m => Node -> m (Dynamic Number)
scrollTopDyn n = do
  initialScroll <- liftEffect $ scrollTop (unsafeCoerce n)
  scrolls <- domEventWithSample (\evt -> scrollTop (unsafeCoerce n)) "scroll" n
  holdDyn initialScroll scrolls
    
-- scrollHeightDyn :: forall m. MonadFRP m => Node -> m (Dynamic Number)
-- scrollHeightDyn n = do
--   w <- window
--   currReq <- liftEffect $ ERef.new =<< Nothing
--   let cancelCurrentReq = ERef.read currReq >>= traverse (\r -> cancelAnimationFrame r w)
--   onCleanup cancelCurrentReq
--   flip requestAnimationFrame w \ts -> do
--     cancelCurrentReq

  
--   initialScroll <- liftEffect $ scrollTop (unsafeCoerce n)
--   scrolls <- domEventWithSample (\evt -> scrollTop (unsafeCoerce n)) "scroll" n
--   holdDyn initialScroll scrolls

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


scrollHeightSample :: forall m. MonadFRP m => Node -> m (Event Number)
scrollHeightSample n = do
  c :: Event Int <- clockMilliseconds 200
  subscribeEvent (scrollHeight (unsafeCoerce n)) (const unit <$> c)

-- A widget showing the contents of a room that the user is currently participating in.
joinedRoomView :: forall m. MonadWidget m => SessionInfo -> RoomId -> JoinedRoom m -> m Unit
joinedRoomView si rId room = do

  liftEffect $ Console.log "Room."

  roomTopBar si rId room.meta

  -- Display a loading spinner at the top of the page whenever new messages are being loaded.
  dynamic_ $ room.loadingMessages <#> \l -> when l pulseSpinner

  messages <- fixFRP \requestMore -> do

    let 
      infscrollStep = 10
      init_load = 25

    demand <- foldDyn (\a b -> b+infscrollStep) init_load requestMore

    subscribeEvent_ (\_ -> Console.log "Requesting more!") requestMore 

    msgs <- room.messages $ demand
    -- List of room events. Keep reference to the node since we need it for some scrolling magic.
    (Tuple msgListNode _) <-
      elClass' "div" "room-messages" $ 
        dynamicList_ msgs $ \devt -> 
          dynamic_ $ devt <#> (viewEvent si room.meta)

    -- Keep track of how far the user has scrolled.
    scrl <- scrollTopDyn msgListNode

    -- If the user is at the bottom of the page and it expands, take the user to the new bottom.

    let 
      scrollToBottom :: Effect Unit
      scrollToBottom = do
        h <- scrollHeight $ unsafeCoerce msgListNode
        setScrollTop h (unsafeCoerce msgListNode)

    -- Not the most elegant, but polling works, I guess?
    msgListHeightSamples <- scrollHeightSample msgListNode
    pastHeightD <- holdPast msgListHeightSamples
    pastScrollD <- holdPast (changed scrl)

    flip subscribeEvent_ msgListHeightSamples \newHeight -> do
      scrlAt <- readDynamic scrl
      pastScrlAt <- fromMaybe 0.0 <$> readDynamic pastScrollD
      listVisibleHeight <- offsetHeight (unsafeCoerce msgListNode)
      pastHeight <- fromMaybe 0.0 <$> readDynamic pastHeightD
      when (newHeight > pastHeight && scrlAt + listVisibleHeight == pastHeight) scrollToBottom

    _ <- liftEffect $ setTimeout 100 scrollToBottom

    scrollHitZero <- filterMapEvent (if _ then Just unit else Nothing) <$> withPast (\p n -> n == 0.0 && p > n) 0.0 (changed scrl)

    -- Requesting more logic.
    let 
      -- Event that fires whenevr the user has hit the top of the page.
      
      -- scrollHitZero filtered by whether new messages are being loaded.
      requestMore_out = filterEvent identity $ sampleAt (scrollHitZero <#> const not) (current room.loadingMessages)

    pure $ Tuple requestMore_out msgs

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
