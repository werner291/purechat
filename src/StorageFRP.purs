module StorageFRP where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Hareactive.Combinators (stepper)
import Hareactive.Interop (subscribe)
import Hareactive.Types (Behavior, Stream, Now)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

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

data StoreStatus a
  = NotStored
  | DecodeError String
  | Stored a

storeStatusToMaybe :: forall a. StoreStatus a -> Maybe a
storeStatusToMaybe (Stored a) = Just a
storeStatusToMaybe _ = Nothing

storageStepperJson :: forall a. EncodeJson a => DecodeJson a => String -> Storage -> Stream (Maybe a) -> Now (Behavior (StoreStatus a))
storageStepperJson key stor updt = do
  let
    encodedUpdates :: Stream (Maybe String)
    encodedUpdates = map (encodeJson >>> stringify) <$> updt

    -- Decode errors are most likely the result of a schema change. 
    -- Just have the user log in again if that's the case.
    -- TODO Maybe some kind of error state instead of Maybe?
    decodeMaybe :: String -> Either String a
    decodeMaybe s = jsonParser s >>= decodeJson

    toStoreStatus :: Maybe String -> StoreStatus a
    toStoreStatus Nothing = NotStored

    toStoreStatus (Just s) = case decodeMaybe s of
      Right a -> Stored a
      Left err -> DecodeError err
  stringStored :: Behavior (Maybe String) <- storageStepper key stor encodedUpdates
  pure $ map toStoreStatus stringStored