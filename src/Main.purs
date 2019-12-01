module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Hareactive.Combinators (shiftCurrent)
import Hareactive.Types (Behavior, Stream)
import LoginComponent (loginPage)
import Purechat.Types (SessionInfo)
import StorageFRP (storageStepperJson, storeStatusToMaybe)
import Turbine (Component, component, dynamic, output, runComponent, use)
import Turbine.HTML as E
import Web.HTML (window)
import Web.HTML.Window (localStorage)

-- import MainPage (mainPage)

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
    session <- map storeStatusToMaybe <$> storageStepperJson "session" storage o.updateSession
    let
      pageToShow :: Behavior (Component {} { updateSession :: Stream (Maybe SessionInfo) })
      pageToShow = do
        se <- session
        pure
          $ case se of
              Nothing -> loginPage `use` (\oo -> { updateSession: Just <$> oo.session })
              Just ses -> (E.text "Username") `use` (\oo -> { updateSession: (mempty :: Stream (Maybe SessionInfo))})
               --mainPage ses `use` (\oo -> { updateSession: const Nothing <$> oo.logout })
    (dynamic pageToShow `use` (\oo -> { updateSession: shiftCurrent (_.updateSession <$> oo) })) `output` {}

main :: Effect Unit
main = runComponent "#mount" app