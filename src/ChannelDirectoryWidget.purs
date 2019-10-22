
module Purechat.ChannelDirectoryWidget (channelDirectory) where

import Prelude

import CustomCombinators (elClass)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Purechat.Types (RoomData, RoomId, unRoomId)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Builder.Class (domEventWithSample, el, elAttr', text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (Dynamic, Event, dynamic, leftmost, switch)
import Specular.FRP.List (dynamicList)

-- Make an arbitrary DOM element clickeable.
elemOnClick :: forall m. MonadWidget m => String -> Attrs -> m Unit -> m (Event Unit)
elemOnClick tagName attrs inner = do
  Tuple node _ <- elAttr' tagName attrs inner
  domEventWithSample (\_ -> pure unit) "click" node

-- A widget showing a short, compact list of all channels the user might currently be interested in.
-- Returns an Event stream of room IDs
channelDirectory :: forall m. MonadWidget m => Dynamic (Map RoomId RoomData) -> m (Event RoomId)
channelDirectory joined_channels =
  
    elClass "div" "channel-directory" $ do
        let 
            clickableLi :: (Tuple RoomId RoomData) -> m (Event RoomId)
            clickableLi (Tuple rId rd) = do
                clicks :: Event Unit <- elemOnClick "li" mempty $ text (unRoomId rId)
                pure $ const rId <$> clicks
            viewrow :: Dynamic (Tuple RoomId RoomData) -> m (Event RoomId)
            viewrow d = switch <$> dynamic (d <#> clickableLi)
        
        el "h2" $ text "Joined rooms"

        (switch <<< map leftmost) <$> el "ul" (dynamicList (Map.toUnfoldable <$> joined_channels) $ viewrow)

        -- el' "h2" $ text "Public channels:"