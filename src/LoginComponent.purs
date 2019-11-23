module Purechat.LoginComponent where

import Prelude

import API.Core (tryLogin)
import Control.Apply (lift3)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, killFiber, runAff, runAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Behavior (sample_)
import FRP.Event (subscribe)
import FRP.Event as FRP.Event
import MyFRP (Dynamic, Widget, buttonOnClick, current, divElement, element, fixWidget, stepDyn, textInputOnInput)

type LoginCredentials
  = { username :: String
    , password :: String
    , homeserver :: String
    }

data AsyncStatus a
  = Running
  | Success a
  | Error Aff.Error

affBehaviorCanceleable :: forall a. Aff a -> Effect { cancel :: Effect Unit, status :: Dynamic (AsyncStatus a) }
affBehaviorCanceleable aff = do
  { event, push } <- FRP.Event.create
  fiber <- runAff push aff
  pure
    { status:
      stepDyn Running
        ( event
            <#> case _ of
                Left e -> Error e
                Right a -> Success a
        )
    , cancel: runAff_ (const $ pure unit) $ killFiber (Aff.error "Request canceled.") fiber
    }

affBehavior :: forall a. Aff a -> Effect (Dynamic (AsyncStatus a))
affBehavior aff = affBehaviorCanceleable aff >>= (\{ status } -> pure status)

-- | 
latestAffEventBehavior :: forall a. FRP.Event.Event (Aff a) -> Effect (Dynamic (Maybe (Dynamic (AsyncStatus a))))
latestAffEventBehavior affs = do
  toCancel <- Ref.new Nothing
  { event, push } <- FRP.Event.create
  _ <- subscribe affs
    ( \aff -> do
        {cancel, status} <- affBehaviorCanceleable aff
        c <- Ref.read toCancel
        case c of
          Just e -> e
          _ -> pure unit
        Ref.write (Just cancel) toCancel 
        push status
    )
  pure $ stepDyn Nothing (Just <$> event)

loginForm :: Widget Unit
loginForm = do
  _ <-
    divElement
      $ fixWidget \loginCmds -> do
          username <- textInputOnInput ""
          element "br" $ pure unit
          password <- textInputOnInput "" --(O.singleton "type" "password")
          element "br" $ pure unit
          homeserver <- textInputOnInput "https://matrix.org"
          element "br" $ pure unit
          tryLoginBtn <- buttonOnClick "Login"

          status <- liftEffect $ latestAffEventBehavior $ sample_ (current $ lift3 tryLogin username password homeserver) loginCmds

          pure
            { input: loginCmds
            , output: (mempty :: FRP.Event.Event Unit)
            }
  pure unit
 -- loginForm :: forall m. WidgetBuilder m => m (Event SessionInfo)   -- loginForm = --   element "div" $ text "Login!"   -- $ affButtonLoopSimplified  --     { ready:   --       \err -> do  --         username <- textInputOnInput "" mempty  --         el_ "br"  --         password <- textInputOnInput "" (O.singleton "type" "password")  --         el_ "br" --         homeserver <- textInputOnInput "https://matrix.org" mempty  --         tryLoginBtn <- buttonOnClick (pure mempty) (text " Login") --         let --           combined :: Dynamic LoginCredentials  --           combined = do  --         pure $ (tagDyn combined tryLoginBtn) <#> (\att -> tryLogin att.username att.password att.homeserver) --     , loading: --       do --         pulseSpinner --         text "Logging in..." --     , success: --       \_ -> do --         text "Login successful!" --     }