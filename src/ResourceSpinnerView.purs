module ResourceSpinnerView (remoteResourceSpinnerView) where

import Prelude

import Data.Maybe (Maybe(..))
import RemoteResource (RemoteResourceStatus(..))
import Hareactive.Types (Behavior)
import Turbine (Component, component, dynamic, output, use)
import Turbine.HTML as E

-- | Provide a view of some remote resource (RemoteResourceStatus)
-- | that is replaced with a loading spinner or status message
-- | depending on load or error status of the resource.
remoteResourceSpinnerView :: forall a o. Behavior (RemoteResourceStatus a) 
                          -> (a -> Component o {})
                          -> Component (Behavior (Maybe o)) {}
remoteResourceSpinnerView res comp =
  let vw :: RemoteResourceStatus a -> Component {} { out :: Behavior (Maybe o) }
      vw resourceStatus =
        case resourceStatus of
          ResourceLoading -> E.text "Loading..." `use` (\oo -> {out:(pure Nothing)})
          ResourceError reason -> E.text ("Failed: " <> reason) `use` (\oo -> {out:(pure Nothing)})
          ResourceLoaded a -> comp a `use` (\oo -> {out:pure (Just oo)})
  in
    component \o -> do
      (dynamic (vw <$> res) `use` (\oo -> {out: oo >>= _.out })) `output` o.out