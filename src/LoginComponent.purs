module Purechat.LoginComponent where

import Prelude

import API (tryLogin)
import Control.Monad.Cleanup (class MonadCleanup)
import CustomCombinators (sampleId)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Foreign.Object as O
import Purechat.Types (SessionInfo)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Element (el_)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (Behavior, Dynamic, Event, changed, current, dynamic_, filterMapEvent, foldDyn, subscribeEvent_)
import Specular.FRP as S
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe)
import Specular.FRP.Async as SA

forEachEvent :: ∀ m a. MonadEffect m ⇒ MonadCleanup m ⇒ Event a → (a → Effect Unit) → m Unit
forEachEvent = flip subscribeEvent_

type LoginCredentials
  = { username :: String
    , password :: String
    , homeserver :: String
    }

loginForm :: forall m. MonadWidget m => m (Event SessionInfo)
loginForm = el "div" do

  username :: (Behavior String) <- current <$> textInputOnInput "" mempty

  el_ "br" []

  password :: (Behavior String) <- current <$> textInputOnInput "" (O.fromFoldable [ Tuple "type" "password" ])

  el_ "br" []

  homeserver :: (Behavior String) <- current <$> textInputOnInput "https://matrix.org" mempty
  tryLoginBtn <- buttonOnClick (pure mempty) (text " Login")

  let
    combined :: Behavior LoginCredentials
    combined = do
          u <- username
          p <- password
          h <- homeserver
          pure { username: u, password: p, homeserver: h }

    loginAttempts :: Event LoginCredentials --  (Aff (Either String LoginToken))
    loginAttempts = sampleId tryLoginBtn combined

    loginAttemptRequests :: Event (Aff (Either String SessionInfo))
    loginAttemptRequests = loginAttempts <#> (\att -> tryLogin att.username att.password att.homeserver)
  
  currentRequest <- foldDyn (\l r -> Just l) Nothing loginAttemptRequests
  loginStatus :: Dynamic (RequestState (Either String SessionInfo)) <- asyncRequestMaybe $ currentRequest

  dynamic_ $ loginStatus <#> \status ->
                case status of
                  SA.NotRequested -> pure unit
                  SA.Loading -> text "Logging in..."
                  SA.Loaded res -> 
                    case res of
                      Left error -> text $ "Failure: " <> error
                      Right token -> text "Token obtained!"

  pure $ filterMapEvent identity $ changed (S.for loginStatus $ \status -> 
    case status of
      Loaded (Right sess) -> Just sess
      _ -> Nothing)
