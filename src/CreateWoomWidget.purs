module Purechat.Widgets.CreateRoomWidget where

import Prelude

import API.Rooms (createRoom)
import CustomCombinators (affButtonLoopSimplified, elClass, pulseSpinner)
import Data.Maybe (Maybe(..))
import Effect.Aff (message)
import Foreign.Object as Object
import Purechat.Types (RoomId, SessionInfo)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (class MonadFRP, Event, tagDyn)

createRoomWidget :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m (Event RoomId)
createRoomWidget si =
  elClass "div" "create-room"
    $ affButtonLoopSimplified
        { ready:
          \er -> do
            case er of
              Just err -> text $ "Failed to create room: " <> message err
              Nothing -> pure unit
            alias <-
              elClass "div" "input-group" do
                text "Canonical alias"
                el "br" $ pure unit
                alias <- textInputOnInput "" Object.empty
                text $ ":" <> si.homeserver
                pure alias
            createClicks <- buttonOnClick (pure Object.empty) $ text "Create room"
            pure $ createRoom si <$> tagDyn alias createClicks
        , loading:
          do
            pulseSpinner
            text "Creating room..."
        , success:
          \_ -> do
            text $ "Created successfully!"
        }
