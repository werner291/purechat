module MyFRP where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node)
import Web.DOM.Document (createTextNode)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.Text as Text
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

-- data Channel m a = Event | Behavior

class
  Monad m <= MonadNetworkBuilder v m | m -> v where
  mkNode :: forall a. (v a -> m (Maybe a)) -> m (Event a)
  connect :: forall a. v a -> m a
  -- set :: forall a. v a -> a -> m Unit

  

newtype NetworkBuilder = MkNetworkBuilder (Effect Unit)

instance networkBuilderImpl :: MonadNetworkBuilder NetworkBuilder where
  mkEvent evt

-- class WidgetBuilder m where
--   text :: String -> m Unit
--   element :: forall a. String -> m a -> m a

-- instance widgetBuilderEffect :: (MonadEffect m, MonadReader Node m) => WidgetBuilder m where
--   text content = do
--     node :: Node <- ask
--     -- Assuming the node is in a document.
--     liftEffect
--       $ do 
--           d <- document =<< window
--           n <- createTextNode content (toDocument d)
--           _ <- appendChild (Text.toNode n) node
--           pure unit
--   element tag content = do
--     node :: Node <- ask
--     -- Assuming the node is in a document.
--     let
--       doc = unsafePartial $ fromJust $ Document.fromNode node
--     n <-
--       liftEffect
--         $ do
--             d <- document =<< window
--             n <- map Element.toNode $ Document.createElement tag (toDocument d)
--             -- Why does this return a Node?
--             _ <- appendChild n node
--             pure n
--     local (const $ n) content

-- newtype WBuilder a
--   = WBuilder (ReaderT Node Effect a)

-- derive newtype instance functorWBuilder :: Functor WBuilder

-- derive newtype instance applyWBuilder :: Apply WBuilder

-- derive newtype instance applicativeWBuilder :: Applicative WBuilder

-- derive newtype instance bindWBuilder :: Bind WBuilder

-- derive newtype instance monadWBuilder :: Monad WBuilder

-- derive newtype instance askWBuilder :: MonadAsk Node WBuilder

-- derive newtype instance readWBuilder :: MonadReader Node WBuilder

-- derive newtype instance effectWBuilder :: MonadEffect WBuilder

-- -- derive newtype instance widgetBuilderWBuilder :: WidgetBuilder WBuilder
-- buildWidgetInNode :: Node -> WBuilder Unit -> Effect Unit
-- buildWidgetInNode n (WBuilder r) = runReaderT r $ n

-- buildWidgetInBody :: WBuilder Unit -> Effect Unit
-- buildWidgetInBody widgetBuilder = do
--   b <- body =<< document =<< window
--   buildWidgetInNode (HTMLElement.toNode $ unsafePartial $ fromJust b) widgetBuilder
