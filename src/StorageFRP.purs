module StorageFRP (localStorageDynamic, localStorageJsonDynamic, StoreStatus(..)) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Specular.FRP (class MonadFRP, Dynamic, Event, holdDyn, subscribeEvent_)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

localStorageDynamic :: forall m. MonadFRP m => String -> Storage -> Event (Maybe String) -> m (Dynamic (Maybe String))
localStorageDynamic key stor updt = do
  fst :: Maybe String <- liftEffect $ getItem key stor
  subscribeEvent_
    ( \mv -> case mv of
        Just v -> setItem key v stor
        Nothing -> removeItem key stor
    )
    updt
  holdDyn fst updt

data StoreStatus a
  = NotStored
  | DecodeError String
  | Stored a

storeStatusToMaybe :: forall a. StoreStatus a -> Maybe a
storeStatusToMaybe (Stored a) = Just a

storeStatusToMaybe _ = Nothing

localStorageJsonDynamic :: forall a m. MonadFRP m => EncodeJson a => DecodeJson a => String -> Storage -> Event (Maybe a) -> m (Dynamic (StoreStatus a))
localStorageJsonDynamic key stor updt = do
  let
    encodedUpdates :: Event (Maybe String)
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
  stringStored :: Dynamic (Maybe String) <- localStorageDynamic key stor encodedUpdates
  pure $ map toStoreStatus stringStored