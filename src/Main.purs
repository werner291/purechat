module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Purechat.LoginComponent (loginForm)
import Purechat.Purechat (primaryView)
import Purechat.Types (SessionInfo)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.FRP (class MonadFRP, Dynamic, Event, dynamic, never, switch)
import Specular.FRP.Fix (fixFRP_)
import StorageFRP (StoreStatus, localStorageJsonDynamic, storeStatusToMaybe)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage)

loginThenMain :: forall m. MonadWidget m => MonadFRP m => m Unit
loginThenMain = 
  let 
    -- showPage :: forall m. MonadWidget m => Maybe SessionInfo -> m (Event SessionInfo)
    showPage s =
      case s of
        Nothing -> 
          loginForm
        Just sess -> do
          primaryView sess
          pure never

    -- loop :: forall m. MonadWidget m => MonadFRP m => Event SessionInfo -> m (Event SessionInfo)
    loop sessionUpdate = do
      storage :: Storage <- liftEffect $ window >>= localStorage
      sess :: Dynamic (StoreStatus SessionInfo) <- localStorageJsonDynamic "session" storage (Just <$> sessionUpdate)
      events :: Dynamic (Event SessionInfo) <- dynamic (storeStatusToMaybe >>> showPage <$> sess)
      pure $ switch events
  in
    fixFRP_ loop
      
  
  
  
main :: Effect Unit
main = runMainWidgetInBody loginThenMain