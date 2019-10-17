
module CustomCombinators where

import Prelude

import Specular.FRP (Behavior, Event, sampleAt)

-- | Fires an event with the current value of the behavior 
-- | whenever an event from the given event stream occurs.
sampleId :: forall a. Event Unit -> Behavior a -> Event a
sampleId e b = sampleAt (const identity <$> e) b