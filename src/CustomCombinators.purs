
module CustomCombinators where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign.Object as Object
import Specular.Dom.Browser (Node, TagName)
import Specular.Dom.Builder.Class (elAttr, elAttr')
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Behavior, Dynamic, Event, WeakDynamic, changed, dynamic, dynamic_, filterMapEvent, fixFRP, holdDyn, holdWeakDyn, never, sampleAt, subscribeEvent_, switch, unWeakDynamic)
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe, fromLoaded)
import Specular.FRP.List (dynamicList, dynamicList_)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

-- | Fires an event with the current value of the behavior 
-- | whenever an event from the given event stream occurs.
sampleId :: forall a. Event Unit -> Behavior a -> Event a
sampleId e b = sampleAt (const identity <$> e) b

elClass :: forall m a. MonadWidget m => TagName -> String -> m a -> m a
elClass tagName cls content = elAttr tagName (Object.fromFoldable [Tuple "class" cls]) content

elClass' :: forall m a. MonadWidget m => TagName -> String -> m a -> m (Tuple Node a)
elClass' tagName cls content = elAttr' tagName (Object.fromFoldable [Tuple "class" cls]) content

localStorageDynamic :: forall m. MonadFRP m => String -> Storage -> Event (Maybe String) -> m (Dynamic (Maybe String))
localStorageDynamic key stor updt = do
    fst :: Maybe String <- liftEffect $ getItem key stor
    subscribeEvent_ (\mv -> case mv of 
                            Just v -> setItem key v stor
                            Nothing -> removeItem key stor) updt
    holdDyn fst updt

data StoreStatus a = NotStored | DecodeError String | Stored a

storeStatusToMaybe :: forall a. StoreStatus a -> Maybe a
storeStatusToMaybe (Stored a) = Just a
storeStatusToMaybe _ = Nothing

localStorageJsonDynamic :: forall a m. MonadFRP m => EncodeJson a => DecodeJson a => String -> Storage -> Event (Maybe a) -> m (Dynamic (StoreStatus a))
localStorageJsonDynamic key stor updt = do
    let encodedUpdates :: Event (Maybe String)
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

dynamicMaybe :: forall a b m. MonadWidget m => Dynamic (Maybe a) -> (Dynamic a -> m b) -> m (Dynamic (Maybe b))
dynamicMaybe dm mkJ = do
    listRes :: Dynamic (Array b) <- dynamicList (Array.fromFoldable <$> dm) mkJ
    pure $ Array.head <$> listRes

dynamicMaybe_ :: forall a m. MonadWidget m => Dynamic (Maybe a) -> (Dynamic a -> m Unit) -> m Unit
dynamicMaybe_ dm mkJ = do
    -- We use 2 separate dynamic widgets here to avoid re-creating 
    -- the view every time we get a new value within the maybe.
    dynamicList_ (Array.fromFoldable <$> dm) mkJ
    -- dynamic_ $ dm
    --     <#> \m -> case m of
    --         Just _ -> pure unit
    --         Nothing -> mkN unit

-- | A widget combinator that represents the common scenario where a is provided with one view,
-- | actions in this view result in some async action (such as a network request). This action
-- | takes some time, during which the user will see some kind of "loading" view.
-- | Finally, the action will result in success or failure.
affButtonLoop :: forall a m. MonadWidget m => MonadFRP m => (RequestState (Either Error a) -> m (Event (Aff a))) -> m (Event a)
affButtonLoop loop = fixFRP $ \(attempts :: Event (Aff a)) -> do
    latestRequest :: WeakDynamic (Aff a) <- holdWeakDyn attempts
    requestStatus :: Dynamic (RequestState (Either Error a)) <- asyncRequestMaybe $ unWeakDynamic $ map try latestRequest
    attempts' <- switch <$> (dynamic $ requestStatus <#> loop)
    pure $ Tuple attempts' (filterMapEvent (\status -> hush =<< fromLoaded status) $ changed requestStatus)

affButtonLoopSimplified :: forall a m. MonadWidget m => MonadFRP m => 
    { ready :: Maybe Error -> m (Event (Aff a)), loading :: m Unit, success :: a -> m Unit }-> m (Event a)
affButtonLoopSimplified {ready, loading, success} = 
    affButtonLoop $ case _ of
    NotRequested -> ready Nothing
    Loading -> loading >>= (const $ pure never)
    Loaded (Left e) -> ready (Just e)
    Loaded (Right a) -> success a >>= (const $ pure never)

pulseSpinner :: forall m. MonadWidget m => m Unit
pulseSpinner = elClass "i" "fas fa-spinner fa-pulse" $ pure unit