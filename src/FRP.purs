module FRP where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, local)
import Data.Maybe (fromJust)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node)
import Web.DOM.Document (createTextNode)
import Web.DOM.Document as Document
import Web.DOM.Element as Element

-- | Like MonadState, except we can dynamically create new variables within the state.
class
  Monad m <= MonadVariables v m | m -> v where
  new :: forall a. a -> m (v a)
  set :: forall a. a -> v a -> m Unit
  get :: forall a. v a -> m a

class
  Monad m <= MonadHold m where
  hold :: forall a. a -> Event a m -> m (Behavior a m)

class
  Monad m <= MonadHoldM m where
  holdM :: forall a. a -> Event (m a) m -> m (Behavior a m)

instance variablesHold :: (Monad m, MonadVariables v m) => MonadHold m where
  hold initial (MkEvent subscribe) = do
    xvar <- (new initial)
    pure $ MkBehavior (get xvar)

newtype Event a m
  = MkEvent (a -> m Unit)

newtype Behavior a m
  = MkBehavior (m a)

newtype Dynamic a m
  = MkDynamic
  { value :: Behavior a m
  , update :: Event Unit m
  }

class WidgetBuilder m where
  text :: String -> m Unit
  element :: forall a. String -> m a -> m a

instance widgetBuilderEffect :: (MonadEffect m, MonadReader Node m) => WidgetBuilder m where
  text content = do
    node :: Node <- ask
    -- Assuming the node is in a document.
    let doc = unsafePartial $ fromJust $ Document.fromNode node
    _ <- liftEffect $ createTextNode content doc
    pure unit

  element tag content = do
    node :: Node <- ask
    -- Assuming the node is in a document.
    let doc = unsafePartial $ fromJust $ Document.fromNode node
    n <- liftEffect $ Document.createElement tag doc
    local (const $ Element.toNode n) content
