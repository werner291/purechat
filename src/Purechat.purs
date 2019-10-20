
module Purechat.Purechat (primaryView) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.ServerFeed (serverState)
import Purechat.Types (RoomData, RoomId, SessionInfo)
import RoomWidget (roomView)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, dynamic_, holdDyn)
    
-- The "primary" widget that is visible once the user is logged in .
primaryView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m Unit
primaryView si = do
    -- Keep a list of channels the user has currently joined
    joined_channels :: Dynamic (Map RoomId RoomData) <- serverState si

    channelPicked <- channelDirectory joined_channels

    -- TODO : Make some kind of multi-window view. Should be easy enough by
    --        just making multiple sub-components here...
    currentChannel <- holdDyn Nothing (Just <$> channelPicked)

    dynamic_ $ currentChannel <#> \rId -> case rId of
      Just rid -> roomView si rid (Map.lookup rid <$> joined_channels)
      Nothing -> pure unit