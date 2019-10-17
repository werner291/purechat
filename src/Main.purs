module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Purechat.LoginComponent (loginForm)
import Purechat.Types (SessionInfo)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.FRP (class MonadFRP, Dynamic, Event, dynamic, foldDyn, never, switch)
import Specular.FRP.Fix (fixFRP_)
import Purechat.Purechat (primaryView)

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
      sess :: Dynamic (Maybe SessionInfo) <- foldDyn (\l _ -> Just l) Nothing sessionUpdate
      events :: Dynamic (Event SessionInfo) <- dynamic (showPage <$> sess)
      pure $ switch events
  in
    fixFRP_ loop
      
  
  
  
main :: Effect Unit
main = runMainWidgetInBody loginThenMain