module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Purechat.LoginComponent (loginForm)
import Purechat.Purechat (primaryView)
import Purechat.Types (SessionInfo)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.FRP (class MonadFRP, Dynamic, dynamic, never, switch)
import Specular.FRP.Fix (fixFRP_)
import StorageFRP (StoreStatus, localStorageJsonDynamic, storeStatusToMaybe)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage)

-- | Top-level component that manages switching the user interface between log and main views.
loginThenMain :: forall m. MonadWidget m => MonadFRP m => m Unit
loginThenMain =
  -- We provide an event loop that updates the current session validity.
  fixFRP_ \sessionUpdate -> do
    -- Get a reference to the local storage.
    storage :: Storage <- liftEffect $ window >>= localStorage
    -- Run the session update loop through local storage so that it can persist between restarts.
    sess :: Dynamic (StoreStatus SessionInfo) <- localStorageJsonDynamic "session" storage (Just <$> sessionUpdate)
    -- Silence any errors into Nothing, show main page vs login page
    -- depending on whether we have an active session or not.
    switch
      <$> dynamic
          ( sess <#> storeStatusToMaybe
              >>> case _ of
                  Nothing -> loginForm
                  Just s -> do
                    primaryView s
                    pure never
          )

main :: Effect Unit
main = runMainWidgetInBody loginThenMain
