module RoomWidget (roomView, joinedRoomView) where

import Prelude

import API (sendMessage)
import API as API
import CustomCombinators (affButtonLoopSimplified, dynamicMaybe_, elClass, elClass', pulseSpinner)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (message)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Purechat.Types (MatrixEvent, MatrixRoomEvent(..), RoomData, RoomId, RoomMembership(..), SessionInfo, unRoomId)
import Specular.Dom.Builder.Class (domEventWithSample, el, el', elAttr, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (checkbox, getTextInputValue, setTextInputValue)
import Specular.FRP (class MonadFRP, Dynamic, Event, current, dynamic_, fixFRP, holdDyn, holdWeakDyn, pull, readBehavior, subscribeDyn_, subscribeEvent_, tagDyn, unWeakDynamic)
import Specular.FRP.Async (asyncRequestMaybe)
import Specular.FRP.List (dynamicList_)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (scrollHeight, setScrollTop)

divClass :: forall m a. MonadWidget m => String -> m a -> m a
divClass cls content = elAttr "div" (Object.fromFoldable [ Tuple "class" cls ]) content

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
    divClass "message-form" $ fixFRP loop

viewEvent :: forall m. MonadWidget m => MatrixEvent MatrixRoomEvent -> m Unit
viewEvent evt =
  elAttr "div" (Object.fromFoldable [ Tuple "class" "message-wrapper" ])
    $ do
        elAttr "p" (Object.fromFoldable [ Tuple "class" "username" ]) $ text evt.sender
        case evt.content of
          Left errMsg -> text errMsg
          Right (Message { body }) -> text body
          Right (Membership { displayname, membership }) ->
            text
              $ case membership of Joined -> displayname <> " joined the room."
          Right (RoomName n) -> text $ evt.sender <> " set the room's name to " <> n
          Right (RoomTopic n) -> text $ evt.sender <> " set the room's topic to " <> n
          Right (RoomCanonicalAlias n) -> text $ evt.sender <> " set the room's canonical alias to " <> n

-- foreign import scrollTo :: Node -> Number -> Effect Unit

holdDynLatestJust :: forall a m. MonadFRP m => Event a -> m (Dynamic (Maybe a))
holdDynLatestJust updt = map unWeakDynamic $ holdWeakDyn updt

-- A widget showing the contents of a room that the user is currently participating in.
joinedRoomView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> RoomId -> Dynamic RoomData -> m Unit
joinedRoomView si rId rd = do
  -- Show the room name. Possibly in the future, add 
  -- possibility of clicking to enable typing in a room ID
  {leave} <- elClass "div" "room-meta" do
    elClass "h2" "room-name" $ dynamic_ $ rd <#> \rs -> (text $ fromMaybe (unRoomId rId) rs.state.display_name)
    leave <- buttonOnClick (pure Object.empty) $ elClass "i" "fas fa-sign-out-alt" (pure unit)
    pure {leave}

  _ <- asyncRequestMaybe =<< (holdDynLatestJust $ (const $ API.leaveRoom si rId) <$> leave) --(holdDyn Nothing (const (Just $ API.leaveRoom si rId) <$> leave))
  --subscribeEvent_ (\_ -> ) leave

  elClass "hr" "roomname-content-set" (pure unit)

  (Tuple msgListNode _) <- elClass' "div" "room-messages"
    $ dynamicList_ ((_.timeline.events) <$> rd)
    $ \devt -> dynamic_ $ devt <#> viewEvent

  autoScroll <- checkbox true Object.empty
  text "Auto-scroll"

  let 
    scrollToBottomCond = do
      autoScrl <- pull $ readBehavior (current autoScroll)
      when autoScrl do
        let elt = unsafeCoerce msgListNode
        h <- scrollHeight elt
        setScrollTop h elt

  liftEffect scrollToBottomCond
  subscribeDyn_  (const scrollToBottomCond) rd 

  msg <- composeMessageWidget
  currentRequest <- holdDyn Nothing (map Just (sendMessage si rId <$> msg))
  result <- asyncRequestMaybe currentRequest
  pure unit

joinRoomView :: forall m. MonadWidget m => SessionInfo -> RoomId -> m Unit
joinRoomView si rId = do
  
  el "p" $ text ("You are currently not participating in room " <> (unRoomId rId) <> " would you like to join it?")
  
  _ <- affButtonLoopSimplified 
        { ready: \err -> do
            case err of 
              Just e -> text $ "Failed to join room: " <> (message e)
              Nothing -> pure unit
            tryJoin <- buttonOnClick (pure mempty) (text "Join room")
            pure $ (const $ API.tryJoinRoom si (unRoomId rId)) <$> tryJoin
        , loading: do
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
  elAttr "div" (Object.fromFoldable [ Tuple "class" "room-view" ]) $ do
    dynamicMaybe_ mrd (joinedRoomView si rId)
    dynamic_ $ mrd <#> case _ of
      Just _ -> pure unit
      Nothing -> joinRoomView si rId

