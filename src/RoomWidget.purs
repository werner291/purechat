module RoomWidget (roomView, joinedRoomView) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Purechat.Types (RoomData, RoomId, SessionInfo, unRoomId)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (class MonadFRP, Dynamic, dynamic_)
import Specular.FRP.List (dynamicList_)

-- A widget showing the contents of a room that the user is currently participating in.
joinedRoomView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> RoomId -> Dynamic RoomData -> m Unit
joinedRoomView si rId rd = do
    dynamicList_ ((_.timeline.events) <$> rd) $ \devt -> text "Some event!"
    
-- A single "room view". Think of this as a browser tab with an address bar that can show any
-- accessible room in the Matrix federation. Note that this widget is applicable regardless of
-- join status. If the user is not in the room, they will be shown join/invite options instead.
-- The room directory separate from the room view. Think of the directory as a "remote control"
-- for this widget/ The room view widget can function independently from the directory.
roomView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> RoomId -> Dynamic (Maybe RoomData) -> m Unit
roomView si rId mrd = do
    
    -- Show the room name. Possibly in the future, add 
    -- possibility of clicking to enable typing in a room ID
    text (unRoomId rId)

    -- We use 2 separate dynamic widgets here to avoid re-creating 
    -- the joined room view every time we get a new message
    dynamicList_ (Array.fromFoldable <$> mrd) (joinedRoomView si rId)
    dynamic_ $ mrd <#> \rd -> case rd of
        Just _ -> pure unit
        Nothing -> buttonOnClick (pure mempty) (text "Join room") >>= const (pure unit)