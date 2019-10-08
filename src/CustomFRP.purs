module CustomFRP where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Hareactive.Combinators (stepper)
import Hareactive.Interop (subscribe)
import Hareactive.Types (Behavior, Stream, Now)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)
import Turbine (Component, use)

-- | A variant of the `stepper` combinator that is initialized with a Storage value.
-- | Whenever the signal produces a value, that value is stored to the Storage.
-- | A value of Nothing will delete the stored value.
storageStepper :: String -> Storage -> Stream (Maybe String) -> Now (Behavior (Maybe String))
storageStepper key storage updateSig = do
  initial <-
    liftEffect $ do
        let
            storeX (Just x) = setItem key x storage
            storeX Nothing = removeItem key storage
        subscribe storeX updateSig
        getItem key storage
  stepper initial updateSig

useall :: forall a o. Component a o -> Component {} a
useall c = map (_.out) ((map (const {}) c) `use` (\oo -> { out: oo }))
