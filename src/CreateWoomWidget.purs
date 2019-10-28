
module Purechat.Widgets.CreateRoomWidget where

import Prelude

import API as API
import CustomCombinators (elClass)
import Data.Either (Either(..), hush)
import Effect.Aff (Aff, Error, message, try)
import Foreign.Object as Object
import Purechat.Types (RoomId, SessionInfo)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (class MonadFRP, Event, WeakDynamic, changed, dynamic_, filterMapEvent, holdWeakDyn, tagDyn, unWeakDynamic)
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe, fromLoaded)

createRoomWidget :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m (Event RoomId)
createRoomWidget si = 
    elClass "div" "create-room" do
  
        -- name <- elClass "div" "input-group" do
        --     text "Name"
        --     el "br" $ pure unit
        --     textInputOnInput "" Object.empty

        alias <- elClass "div" "input-group" do
            text "Canonical alias"
            el "br" $ pure unit
            alias <- textInputOnInput "" Object.empty
            text $ ":" <> si.homeserver
            pure alias

        createClicks <- buttonOnClick (pure Object.empty) $ text "Create room"

        currentRequest :: WeakDynamic (Aff (Either Error RoomId)) <- 
            holdWeakDyn $ (try <<< API.createRoom si <$> tagDyn alias createClicks)

        requestStatus <- asyncRequestMaybe $ unWeakDynamic currentRequest

        dynamic_ $ requestStatus <#> case _ of
            NotRequested -> pure unit
            Loading -> do
                elClass "i" "fas fa-spinner" $ pure unit
                text "Creating room..."
            Loaded (Left err) -> 
                text $ "Failed to create room: " <> message err
            Loaded (Right rId) -> 
                text $ "Created successfully!"
        
        pure $ filterMapEvent (\status -> hush =<< fromLoaded status) $ changed requestStatus