module Old.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Hareactive.Combinators (shiftCurrent)
import Hareactive.Types (Behavior, Stream)
import LoginComponent (loginPage)
import Turbine (Component, component, dynamic, output, runComponent, use)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Purechat.Types (LoginToken(..), SessionInfo, unToken)
import CustomFRP (storageStepper)
import MainPage (mainPage)

-- | The entry-point application component, whose primary function it is to switch around
-- | between the login / registration / main activities.
app :: Component {} {}
app =
  component \o -> do
    storage <-
      liftEffect
        $ do
            w <- window
            localStorage w
    token <- storageStepper "token" storage ((_ >>= \ss -> Just (unToken ss.token)) <$> o.updateSession)
    homeserver <- storageStepper "homeserver" storage ((_ >>= \ss -> (Just ss.homeserver)) <$> o.updateSession)
    let
      session = do
        t <- token
        h <- homeserver
        pure
          $ do
              tt <- t
              hh <- h
              pure
                { token: (LoginToken tt)
                , homeserver: hh
                }
    let
      pageToShow :: Behavior (Component {} { updateSession :: Stream (Maybe SessionInfo) })
      pageToShow = do
        se <- session
        pure
          $ case se of
              Nothing -> loginPage `use` (\oo -> { updateSession: Just <$> oo.session })
              Just ses -> mainPage ses `use` (\oo -> { updateSession: const Nothing <$> oo.logout })
    (dynamic pageToShow `use` (\oo -> { updateSession: shiftCurrent (_.updateSession <$> oo) })) `output` {}

main :: Effect Unit
main = runComponent "#mount" app
