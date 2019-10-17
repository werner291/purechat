
module Purechat.Purechat (primaryView) where

import Prelude

import API (SyncPollResult, pollSyncProducer)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Purechat.Types (SessionInfo, RoomData)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Builder.Class (domEventWithSample, el, elAttr', text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, Event, dynamic, dynamic_, foldDyn, leftmost, newEvent, switch)
import Specular.FRP.Async (startAff)
import Specular.FRP.List (dynamicList, dynamicList_)

syncFeed :: forall m. MonadFRP m => SessionInfo -> m (Event SyncPollResult) 
syncFeed si = do
    evt <- newEvent
    startAff $ pollSyncProducer si (\update -> liftEffect $ evt.fire update)
    pure evt.event

serverState :: forall m. MonadFRP m => SessionInfo -> m (Dynamic (Map String RoomData)) 
serverState si = do
    let combine updt rooms = Map.unionWith (<>) rooms (updt.rooms.join)
    feed <- syncFeed si
    foldDyn combine (Map.empty) feed

elemOnClick :: forall m. MonadWidget m => String -> Attrs -> m Unit -> m (Event Unit)
elemOnClick tagName attrs inner = do
  Tuple node _ <- elAttr' tagName attrs inner
  domEventWithSample (\_ -> pure unit) "click" node

primaryView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m Unit
primaryView si = do
    st :: Dynamic (Map String RoomData) <- serverState si
    
    let clickableLi :: (Tuple String RoomData) -> m (Event String)
        clickableLi (Tuple rId rd) = do
            clicks :: Event Unit <- elemOnClick "li" mempty $ text rId
            pure $ const rId <$> clicks
        viewrow :: Dynamic (Tuple String RoomData) -> m (Event String)
        viewrow d = switch <$> dynamic (d <#> clickableLi)

    channelPicked :: Event String <- (switch <<< map leftmost) <$> el "ul" (dynamicList (Map.toUnfoldable <$> st) $ viewrow)

    selectedChannels :: Dynamic (Array String) <- foldDyn Array.cons mempty channelPicked

    el "ul" (dynamicList_ selectedChannels (\dynName -> dynamic_ ((el "li"  <<< text) <$> dynName)))

     -- (entryD <#> (\(Tuple rId rd) -> text rId))
  