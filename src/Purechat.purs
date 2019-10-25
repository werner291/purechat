
module Purechat.Purechat (primaryView) where

import Prelude

import CustomCombinators (dynamicMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.ServerFeed (serverState)
import Purechat.Types (RoomData, RoomId, SessionInfo)
import RoomWidget (roomView)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, dynamic_, holdDyn)
    
-- The "primary" widget that is visible once the user is logged in .
primaryView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m Unit
primaryView si = do
    -- Keep a list of channels the user has currently joined
    joined_channels_maybe :: Dynamic (Maybe (Map RoomId RoomData)) <- serverState si

    _ <- dynamicMaybe joined_channels_maybe $ (\joined_channels -> do
    
      channelPicked <- channelDirectory joined_channels

      -- TODO : Make some kind of multi-window view. Should be easy enough by
      --        just making multiple sub-components here...
      currentChannel <- holdDyn Nothing (Just <$> channelPicked)

      dynamic_ $ currentChannel <#> \rId -> case rId of
        Just rid -> roomView si rid (Map.lookup rid <$> joined_channels)
        Nothing -> text "Welcome! PLease select a room to get started.")

    dynamic_ $ joined_channels_maybe <#> \jcm -> case jcm of
      Just _ -> pure unit
      Nothing -> text "Loading channels, please stand by..."

    pure unit