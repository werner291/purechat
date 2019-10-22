module RoomWidget (roomView, joinedRoomView) where

import Prelude
import API (sendMessage)
import CustomCombinators (elClass)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Purechat.Types (MatrixEvent, MatrixRoomEvent(..), RoomData, RoomId, RoomMembership(..), SessionInfo, unRoomId)
import Specular.Dom.Builder.Class (domEventWithSample, el', elAttr, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (class MonadFRP, Dynamic, Event, dynamic_, fixFRP, holdDyn, subscribeEvent_, tagDyn)
import Specular.FRP.Async (asyncRequestMaybe)
import Specular.FRP.List (dynamicList_)

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

-- A widget showing the contents of a room that the user is currently participating in.
joinedRoomView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> RoomId -> Dynamic RoomData -> m Unit
joinedRoomView si rId rd = do
  -- Show the room name. Possibly in the future, add 
  -- possibility of clicking to enable typing in a room ID
  elClass "div" "room-name" $ dynamic_ $ rd <#> \rs -> (text $ fromMaybe (unRoomId rId) rs.state.display_name)
  elClass "div" "room-messages"
    $ dynamicList_ ((_.timeline.events) <$> rd)
    $ \devt -> dynamic_ $ devt <#> viewEvent
  msg <- composeMessageWidget
  currentRequest <- holdDyn Nothing (map Just (sendMessage si rId <$> msg))
  result <- asyncRequestMaybe currentRequest
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
        -- We use 2 separate dynamic widgets here to avoid re-creating 
        -- the joined room view every time we get a new message
        dynamicList_ (Array.fromFoldable <$> mrd) (joinedRoomView si rId)
        dynamic_ $ mrd
          <#> \rd -> case rd of
              Just _ -> pure unit
              Nothing -> buttonOnClick (pure mempty) (text "Join room") >>= const (pure unit)
