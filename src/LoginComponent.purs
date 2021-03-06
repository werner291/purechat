module Purechat.LoginComponent where

import Prelude

import API.Core (tryLogin)
import Control.Monad.Cleanup (class MonadCleanup)
import CustomCombinators (affButtonLoopSimplified, elClass, pulseSpinner)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object as O
import Purechat.Types (SessionInfo)
import Specular.Dom.Builder.Class (el, el_, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (Dynamic, Event, subscribeEvent_, tagDyn)

forEachEvent :: ∀ m a. MonadEffect m ⇒ MonadCleanup m ⇒ Event a → (a → Effect Unit) → m Unit
forEachEvent = flip subscribeEvent_

type LoginCredentials
  = { username :: String
    , password :: String
    , homeserver :: String
    }

loginForm :: forall m. MonadWidget m => m (Event SessionInfo)
loginForm =
  elClass "div" "login-form"
    $ affButtonLoopSimplified
        { ready:
          \err -> do
            username <- el "label" do
              elClass "span" "label-text" $ text "Username"
              textInputOnInput "" mempty
            
            password <- el "label" do
              elClass "span" "label-text" $ text "Password"
              textInputOnInput "" (O.singleton "type" "password")
            
            homeserver <- el "label" do
              elClass "span" "label-text" $ text "Homeserver"
              textInputOnInput "https://matrix.org" mempty

            tryLoginBtn <- buttonOnClick (pure mempty) (text "Log in")

            let
              combined :: Dynamic LoginCredentials
              combined = do
                u <- username
                p <- password
                h <- homeserver
                pure { username: u, password: p, homeserver: h }
            pure $ (tagDyn combined tryLoginBtn) <#> (\att -> tryLogin att.username att.password att.homeserver)
        , loading:
          do
            pulseSpinner
            text "Logging in..."
        , success:
          \_ -> do
            text "Login successful!"
        }
