module Purechat.LoginComponent where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object as O
import MyFRP (class WidgetBuilder, Event, element, text)
import Purechat.Types (SessionInfo)

type LoginCredentials
  = { username :: String
    , password :: String
    , homeserver :: String
    }

loginForm :: forall m. WidgetBuilder m => m (Event SessionInfo)
loginForm =
  element "div" $ text "Login!"
    -- $ affButtonLoopSimplified
    --     { ready:
    --       \err -> do
    --         username <- textInputOnInput "" mempty
    --         el_ "br"
    --         password <- textInputOnInput "" (O.singleton "type" "password")
    --         el_ "br"
    --         homeserver <- textInputOnInput "https://matrix.org" mempty
    --         tryLoginBtn <- buttonOnClick (pure mempty) (text " Login")
    --         let
    --           combined :: Dynamic LoginCredentials
    --           combined = do
    --             u <- username
    --             p <- password
    --             h <- homeserver
    --             pure { username: u, password: p, homeserver: h }
    --         pure $ (tagDyn combined tryLoginBtn) <#> (\att -> tryLogin att.username att.password att.homeserver)
    --     , loading:
    --       do
    --         pulseSpinner
    --         text "Logging in..."
    --     , success:
    --       \_ -> do
    --         text "Login successful!"
    --     }
