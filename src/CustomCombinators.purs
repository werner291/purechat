
module CustomCombinators where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartialBecause)
import Specular.Dom.Browser (TagName)
import Specular.Dom.Builder.Class (elAttr)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Behavior, Dynamic, Event, holdDyn, sampleAt, subscribeEvent_)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

-- | Fires an event with the current value of the behavior 
-- | whenever an event from the given event stream occurs.
sampleId :: forall a. Event Unit -> Behavior a -> Event a
sampleId e b = sampleAt (const identity <$> e) b

elClass :: forall m a. MonadWidget m => TagName -> String -> m a -> m a
elClass tagName cls content = elAttr tagName (Object.fromFoldable [Tuple "class" cls]) content

localStorageDynamic :: forall m. MonadFRP m => String -> Storage -> Event (Maybe String) -> m (Dynamic (Maybe String))
localStorageDynamic key stor updt = do
    fst :: Maybe String <- liftEffect $ getItem key stor
    subscribeEvent_ (\mv -> case mv of 
                            Just v -> setItem key v stor
                            Nothing -> removeItem key stor) updt
    holdDyn fst updt

unsafeDecodeReason :: String
unsafeDecodeReason = 
    "Values should only be able to get in local storage if " <>
        " they're the result of a JSON stringify operation. " <>
        "If not, something is seriously wrong."

localStorageJsonDynamic :: forall a m. MonadFRP m => EncodeJson a => DecodeJson a => String -> Storage -> Event (Maybe a) -> m (Dynamic (Maybe a))
localStorageJsonDynamic key stor updt = do
    let encodedUpdates :: Event (Maybe String)
        encodedUpdates = map (encodeJson >>> stringify) <$> updt
        unsafeDecode :: String -> a
        unsafeDecode s = unsafePartialBecause "Values should only be able to get in local storage if they're the result of a JSON stringify operation. If not, something is seriously wrong." $ fromRight $ (jsonParser s >>= decodeJson)
    stringStored :: Dynamic (Maybe String) <- localStorageDynamic key stor encodedUpdates
    pure $ map unsafeDecode <$> stringStored