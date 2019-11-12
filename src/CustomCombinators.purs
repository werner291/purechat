module CustomCombinators where

import Prelude
import Control.Monad.Cleanup (onCleanup)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, for_)
import Data.List.Lazy (List(..))
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Gen (genDigitString)
import Data.Traversable (class Traversable, for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign.Object as Object
import Specular.Dom.Browser (Node, TagName, Attrs)
import Specular.Dom.Builder.Class (domEventWithSample, elAttr, elAttr')
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Behavior, Dynamic, Event(..), Pull, WeakDynamic, changed, dynamic, filterMapEvent, fixFRP, holdDyn, holdWeakDyn, never, newDynamic, newEvent, sampleAt, subscribeEvent_, switch, unWeakDynamic)
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe, fromLoaded)
import Specular.FRP.List (dynamicList, dynamicList_)
import Specular.Internal.Effect (newRef, readRef, writeRef)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

-- | Fires an event with the current value of the behavior 
-- | whenever an event from the given event stream occurs.
sampleId :: forall a. Event Unit -> Behavior a -> Event a
sampleId e b = sampleAt (const identity <$> e) b

elClass :: forall m a. MonadWidget m => TagName -> String -> m a -> m a
elClass tagName cls content = elAttr tagName (Object.fromFoldable [ Tuple "class" cls ]) content

elClass' :: forall m a. MonadWidget m => TagName -> String -> m a -> m (Tuple Node a)
elClass' tagName cls content = elAttr' tagName (Object.fromFoldable [ Tuple "class" cls ]) content

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

dynamicMaybe :: forall a b m. MonadWidget m => Dynamic (Maybe a) -> (Dynamic a -> m b) -> m (Dynamic (Maybe b))
dynamicMaybe dm mkJ = do
  listRes :: Dynamic (Array b) <- dynamicList (Array.fromFoldable <$> dm) mkJ
  pure $ Array.head <$> listRes

dynamicMaybe_ :: forall a m. MonadWidget m => Dynamic (Maybe a) -> (Dynamic a -> m Unit) -> m Unit
dynamicMaybe_ dm mkJ = do
  dynamicList_ (Array.fromFoldable <$> dm) mkJ

-- | A widget combinator that represents the common scenario where a is provided with one view,
-- | actions in this view result in some async action (such as a network request). This action
-- | takes some time, during which the user will see some kind of "loading" view.
-- | Finally, the action will result in success or failure.
affButtonLoop :: forall a m. MonadWidget m => MonadFRP m => (RequestState (Either Error a) -> m (Event (Aff a))) -> m (Event a)
affButtonLoop loop =
  fixFRP
    $ \(attempts :: Event (Aff a)) -> do
        latestRequest :: WeakDynamic (Aff a) <- holdWeakDyn attempts
        requestStatus :: Dynamic (RequestState (Either Error a)) <- asyncRequestMaybe $ unWeakDynamic $ map try latestRequest
        attempts' <- switch <$> (dynamic $ requestStatus <#> loop)
        pure $ Tuple attempts' (filterMapEvent (\status -> hush =<< fromLoaded status) $ changed requestStatus)

affButtonLoopSimplified ::
  forall a m.
  MonadWidget m =>
  { ready :: Maybe Error -> m (Event (Aff a)), loading :: m Unit, success :: a -> m Unit } -> m (Event a)
affButtonLoopSimplified { ready, loading, success } =
  affButtonLoop
    $ case _ of
        NotRequested -> ready Nothing
        Loading -> loading >>= (const $ pure never)
        Loaded (Left e) -> ready (Just e)
        Loaded (Right a) -> success a >>= (const $ pure never)

pulseSpinner :: forall m. MonadWidget m => m Unit
pulseSpinner = elClass "i" "fas fa-spinner fa-pulse" $ pure unit

holdDynLatestJust :: forall a m. MonadFRP m => Event a -> m (Dynamic (Maybe a))
holdDynLatestJust updt = map unWeakDynamic $ holdWeakDyn updt

-- Make an arbitrary DOM element clickeable.
elemOnClick :: forall m. MonadWidget m => String -> Attrs -> m Unit -> m (Event Unit)
elemOnClick tagName attrs inner = do
  Tuple node _ <- elAttr' tagName attrs inner
  domEventWithSample (\_ -> pure unit) "click" node

fanOut :: forall f k v m. MonadFRP m => Ord k => Traversable f => Event (f (Tuple k v)) -> m (Dynamic (Map k (Event v)))
fanOut ev = do
  { dynamic, read, set } <- newDynamic Map.empty
  flip subscribeEvent_ ev
    $ \(tuples :: f (Tuple k v)) ->
        for_ tuples
          $ \(Tuple k v) -> do
              st <- read
              case lookup k st of
                Just { event, fire } -> fire v
                Nothing -> do
                  { event, fire } <- newEvent
                  set $ Map.insert k { event, fire } st
                  fire v
  pure $ dynamic <#> map _.event

fanIn ::
  forall a m.
  MonadFRP m =>
  (List (Dynamic a) -> Dynamic a) ->
  m
    { subscribe :: Dynamic a -> m (Dynamic a)
    , output :: Dynamic a
    }
fanIn combiner = do
  { dynamic, set, read } <- newDynamic (Map.empty :: Map Int (Dynamic a))

  let
    subscribe :: Dynamic a -> m (Dynamic a)
    subscribe inp =  do
      subs <- liftEffect read
      let
        k = Maybe.fromMaybe 0 (Map.findMax subs <#> _.key + 1)
      liftEffect $ set $ Map.insert k inp subs
      onCleanup
        $ do
            subs <- read
            set $ Map.delete k subs
      pure dynamic

    output = combiner =<< (Map.values <$> dynamic)

  pure { subscribe, output }
