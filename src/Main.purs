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
import Turbine as T
import Turbine.HTML as E
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Purechat.Purechat (primaryPage)

-- | The entry-point application component, whose primary function it is to switch around
-- | between the login / registration / main activities.
app :: T.Component {} {}
app =
  component \o -> do
    storage <-
      liftEffect
        $ do
            w <- window
            localStorage w
    session <- map storeStatusToMaybe <$> storageStepperJson "session" storage o.updateSession
    let
      pageToShow :: Behavior (T.Component {} { updateSession :: Stream (Maybe SessionInfo) })
      pageToShow = do
        se <- session
        pure
          $ case se of
              Nothing -> loginPage `use` (\oo -> { updateSession: Just <$> oo.session })
              Just ses -> primaryPage ses `use` (\oo -> { updateSession: const Nothing <$> oo.logout })
    (dynamic pageToShow `use` (\oo -> { updateSession: shiftCurrent (_.updateSession <$> oo) })) `output` {}

main :: Effect Unit
main = runComponent "#mount" app