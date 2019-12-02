module CustomCombinators where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, try)
import Effect.Exception (Error)
import Foreign.Object as Object
import Specular.Dom.Browser (Node, TagName, Attrs)
import Specular.Dom.Builder.Class (domEventWithSample, elAttr, elAttr')
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Behavior, Dynamic, Event, WeakDynamic, changed, dynamic, filterMapEvent, fixFRP, foldDynMaybe, holdWeakDyn, never, sampleAt, switch, unWeakDynamic)
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe, fromLoaded)

-- | Fires an event with the current value of the behavior 
-- | whenever an event from the given event stream occurs.
sampleId :: forall a. Event Unit -> Behavior a -> Event a
sampleId e b = sampleAt (const identity <$> e) b

elClass :: forall m a. MonadWidget m => TagName -> String -> m a -> m a
elClass tagName cls content = elAttr tagName (Object.fromFoldable [ Tuple "class" cls ]) content

elClass' :: forall m a. MonadWidget m => TagName -> String -> m a -> m (Tuple Node a)
elClass' tagName cls content = elAttr' tagName (Object.fromFoldable [ Tuple "class" cls ]) content

-- Similar to Maybe, but more semantically precise way
-- of signaling that a resource is either loading or currently available.
data RemoteResourceView a
  = RRLoading
  | RRLoaded a

remoteLoadingView :: forall a b m. MonadWidget m => Dynamic (RemoteResourceView a) -> m Unit -> (a -> m b) -> m (Dynamic (Maybe b))
remoteLoadingView d loadingView loadedView =
  dynamic $ d
    <#> case _ of
        RRLoaded a -> loadedView a <#> Just
        RRLoading -> loadingView <#> const Nothing

toLoadedUpdates :: forall a m. MonadFRP m => Event a -> m (Dynamic (RemoteResourceView a))
toLoadedUpdates updt =
  let 
    _step :: a -> RemoteResourceView a -> Maybe (RemoteResourceView a)
    _step a (RRLoaded _) = Nothing
    _step a RRLoading = Just (RRLoaded a)
  in foldDynMaybe _step RRLoading updt

-- | Shorthand combinator to extract a value `a` from a `(Dynamic (Maybe a))`
dynamicMaybe :: forall a b m. MonadWidget m => Dynamic (Maybe a) -> (a -> m b) -> m (Dynamic (Maybe b))
dynamicMaybe dm mkJ = do
  dynamic $ dm
    <#> case _ of
        Just a -> mkJ a <#> Just
        Nothing -> pure Nothing

-- listRes :: Dynamic (Array b) <- dynamicList (Array.fromFoldable <$> dm) mkJ
-- pure $ Array.head <$> listRes
dynamicMaybe_ :: forall a m. MonadWidget m => Dynamic (Maybe a) -> (a -> m Unit) -> m Unit
dynamicMaybe_ dm mkJ = do
  _ <- dynamicMaybe dm mkJ
  pure unit

bridgeEventOverMaybe :: forall a. Dynamic (Maybe (Event a)) -> Event a
bridgeEventOverMaybe d = switch (d <#> fromMaybe never)

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
