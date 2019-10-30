module Purechat.ChannelDirectoryWidget (channelDirectory) where

import Prelude

import CustomCombinators (elClass, elemOnClick)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Purechat.Types (RoomData, RoomId, mkRoomId, unRoomId)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Builder.Class (domEventWithSample, el, elAttr', text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (textInput, textInputValueEventOnEnter)
import Specular.FRP (Dynamic, Event, dynamic, leftmost, never, switch)
import Specular.FRP.List (dynamicList)

searchBar :: forall m. MonadWidget m => m (Event String)
searchBar = do
  inpt <- textInput { initialValue: ""
                    , attributes: pure (Object.fromFoldable [ Tuple "placeholder" "Find room..." ])
                    , setValue: never
                    }
    
  textInputValueEventOnEnter inpt
  

-- A widget showing a short, compact list of all channels the user might currently be interested in.
-- Returns an Event stream of room IDs
channelDirectory :: forall m. MonadWidget m => Dynamic (Map RoomId RoomData) -> m (Event RoomId)
channelDirectory joined_channels =
  elClass "div" "channel-directory" do

    directEnterName :: Event String <- searchBar
    
    let
      clickableLi :: (Tuple RoomId RoomData) -> m (Event RoomId)
      clickableLi (Tuple rId rd) = do
        clicks :: Event Unit <- elemOnClick "li" mempty $ text (fromMaybe (unRoomId rId) rd.state.display_name)
        pure $ const rId <$> clicks

      viewrow :: Dynamic (Tuple RoomId RoomData) -> m (Event RoomId)
      viewrow d = switch <$> dynamic (d <#> clickableLi)

    el "h2" $ text "Joined rooms"
    pickedFromJoined <- (switch <<< map leftmost) <$> el "ul" (dynamicList (Map.toUnfoldable <$> joined_channels) $ viewrow)

    pure $ leftmost [(map mkRoomId directEnterName), pickedFromJoined]
 -- el' "h2" $ text "Public channels:"