module RoomWidget (roomView, joinedRoomView) where

import Prelude
import API.Core (sendMessage)
import API.Rooms (leaveRoom, tryJoinRoom)
import CustomCombinators (affButtonLoopSimplified, dynamicMaybe_, elClass, elClass', pulseSpinner)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (message)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.Types (MatrixEvent, MatrixRoomEvent(..), RoomData, RoomId, RoomMembership(..), SessionInfo, UserProfile, unRoomId, unUserId)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (domEventWithSample, el, el', elAttr, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (checkbox, getTextInputValue, setTextInputValue)
import Specular.FRP (class MonadFRP, Dynamic, Event, current, dynamic_, fixFRP, holdDyn, pull, readBehavior, subscribeDyn_, subscribeEvent_, tagDyn)
import Specular.FRP.Async (asyncRequestMaybe)
import Specular.FRP.List (dynamicList_)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (scrollHeight, setScrollTop)

textareaOnChangeWithReset :: forall m. MonadWidget m => Event Unit -> m (Dynamic String)
textareaOnChangeWithReset reset = do
  Tuple node _ <- el' "textarea" (pure unit)
  changed <- domEventWithSample (\_ -> getTextInputValue node) "change" node
  subscribeEvent_ (\_ -> setTextInputValue node "") reset
  holdDyn "" changed

composeMessageWidget :: forall m. MonadWidget m => m (Event String)
composeMessageWidget =
  let
    loop :: forall mm. MonadWidget mm => MonadFRP mm => Event Unit -> mm (Tuple (Event Unit) (Event String))
    loop reset = do
      composeMessage :: Dynamic String <- textareaOnChangeWithReset reset
      sendBtnClicked :: Event Unit <- buttonOnClick (pure mempty) (text "Send")
      let
        outbox = tagDyn composeMessage sendBtnClicked

        resetOut = const unit <$> outbox
      pure $ Tuple resetOut outbox
  in
    elClass "div" "message-form" $ fixFRP loop

viewEvent :: forall m. MonadWidget m => SessionInfo -> Dynamic RoomData -> MatrixEvent MatrixRoomEvent -> m Unit
viewEvent si drd evt =
  elAttr "div" (Object.fromFoldable [ Tuple "class" "message-wrapper" ])
    $ do
        dynamic_ $ drd
          <#> \rd -> do
              let
                sender_profile :: Maybe UserProfile
                sender_profile = Map.lookup evt.sender rd.members
                sender_displayname = case sender_profile of
                  Just {displayname:(Just n)} -> n
                  _ -> unUserId evt.sender
              showAvatarOrDefault si (sender_profile >>= (\p -> p.avatar_url))
              elClass "div" "msg" do
                elAttr "p" (Object.fromFoldable [ Tuple "class" "username" ]) $ text sender_displayname
                elAttr "p" (Object.fromFoldable [ Tuple "class" "message" ])
                  $ case evt.content of
                      Left errMsg -> text errMsg
                      Right (Message { body }) -> text body
                      Right (Membership { profile, membership }) ->
                        text
                          $ case membership of Joined -> sender_displayname <> " joined the room."
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

autoScrollBar :: forall m. MonadWidget m => Node -> Dynamic RoomData -> m Unit
autoScrollBar msgListNode rd = do
  -- A horizontal strip of space reserved for the checkbox that causes the view to auto-scroll down.
  elClass "div" "auto-scoll-bar" do
    autoScroll <- checkbox true Object.empty
    text "Auto-scroll"
    let
      elt = unsafeCoerce msgListNode

      scrollToBottomCond = do
        h <- scrollHeight elt
        setScrollTop h elt
    liftEffect do
      autoScrl <- pull $ readBehavior (current autoScroll)
      when autoScrl scrollToBottomCond
    subscribeDyn_ (const scrollToBottomCond) rd

-- A widget showing the contents of a room that the user is currently participating in.
joinedRoomView :: forall m. MonadWidget m => SessionInfo -> RoomId -> Dynamic RoomData -> m Unit
joinedRoomView si rId rd = do
  -- Show the room name. Possibly in the future, add 
  -- possibility of clicking to enable typing in a room ID
  elClass "div" "room-meta" do
    elClass "h2" "room-name" $ dynamic_ $ rd <#> \rs -> (text rs.display_name)
    leaveButton si rId
  -- A simple horizontal line separating the meta-area from the content.
  elClass "hr" "roomname-content-set" (pure unit)
  -- List of room events
  (Tuple msgListNode _) <-
    elClass' "div" "room-messages"
      $ dynamicList_ (_.timeline <$> rd)
      $ \devt -> dynamic_ $ devt <#> (viewEvent si rd)
  -- The checkbox with label to auto-scroll to the bottom.
  -- SIDE EFFECTS: this widget affects the message list!
  autoScrollBar msgListNode rd
  -- Message composition widget, which will produce message events whenever "send" is clicked.
  msg <- composeMessageWidget
  currentRequest <- holdDyn Nothing (map Just (sendMessage si rId <$> msg))
  result <- asyncRequestMaybe currentRequest
  pure unit

joinRoomView :: forall m. MonadWidget m => SessionInfo -> RoomId -> m Unit
joinRoomView si rId = do
  el "p" $ text ("You are currently not participating in room " <> (unRoomId rId) <> " would you like to join it?")
  _ <-
    affButtonLoopSimplified
      { ready:
        \err -> do
          case err of
            Just e -> text $ "Failed to join room: " <> (message e)
            Nothing -> pure unit
          tryJoin <- buttonOnClick (pure mempty) (text "Join room")
          pure $ (const $ tryJoinRoom si (unRoomId rId)) <$> tryJoin
      , loading:
        do
          pulseSpinner
          text "Joining room..."
      , success: \_ -> text $ "Room successfully joined!"
      }
  pure unit

-- A single "room view". Think of this as a browser tab with an address bar that can show any
-- accessible room in the Matrix federation. Note that this widget is applicable regardless of
-- join status. If the user is not in the room, they will be shown join/invite options instead.
-- The room directory separate from the room view. Think of the directory as a "remote control"
-- for this widget/ The room view widget can function independently from the directory.
roomView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> RoomId -> Dynamic (Maybe RoomData) -> m Unit
roomView si rId mrd =
  elAttr "div" (Object.fromFoldable [ Tuple "class" "room-view" ])
    $ do
        dynamicMaybe_ mrd (joinedRoomView si rId)
        dynamic_ $ mrd
          <#> case _ of
              Just _ -> pure unit
              Nothing -> joinRoomView si rId
