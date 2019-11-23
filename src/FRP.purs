module MyFRP
  ( Widget
  , Dynamic
  , current
  , updates
  , stepDyn
  , foldDyn
  , textNode
  , text
  , element
  , elementNode
  , divElement
  , runWidgetInBody
  , runWidgetInNode
  , textInputOnInput
  , buttonOnClick
  , fixWidget
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, ask, lift, local, runReaderT)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (error)
import FRP.Behavior (step)
import FRP.Behavior as FB
import FRP.Event (subscribe)
import FRP.Event as FE
import Partial.Unsafe (unsafePartial)
import Web.DOM (Element, Node, Text)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.Text as Text
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (HTMLButtonElement, HTMLInputElement, window)
import Web.HTML.HTMLButtonElement as HTMLButtonElement
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement (fromElement)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window (document)

newtype Dynamic a
  = MkDynamic
  { current :: FB.Behavior a
  , updates :: FE.Event Unit
  }

current :: forall a. Dynamic a -> FB.Behavior a
current (MkDynamic d) = d.current

updates :: forall a. Dynamic a -> FE.Event Unit
updates (MkDynamic d) = d.updates

stepDyn :: forall a. a -> FE.Event a -> Dynamic a
stepDyn init update =
  MkDynamic
    { current: step init update
    , updates: const unit <$> update
    }

foldDyn :: forall a b. b -> (a -> b -> b) -> FE.Event a -> Dynamic b
foldDyn init step update =
  MkDynamic
    { current: FB.unfold step update init
    , updates: const unit <$> update
    }

instance functorDynamic :: Functor Dynamic where
  map f (MkDynamic d) = MkDynamic d { current = map f d.current }

instance applyDynamic :: Apply Dynamic where
  apply (MkDynamic f) (MkDynamic a) =
    MkDynamic
      { current: apply f.current a.current
      , updates: f.updates <> a.updates
      }

instance applicativeDynamic :: Applicative Dynamic where
  pure a =
    MkDynamic
      { current: pure a
      , updates: mempty
      }

newtype Widget a
  = MkWidget (ReaderT Node Effect a)

unWidget :: forall a. Widget a -> ReaderT Node Effect a
unWidget (MkWidget m) = m

derive newtype instance functorWidget :: Functor Widget

derive newtype instance applicativeWidget :: Applicative Widget

derive newtype instance applyWidget :: Apply Widget

derive newtype instance bindWidget :: Bind Widget

derive newtype instance monadWidget :: Monad Widget

derive newtype instance monadEffectWidget :: MonadEffect Widget

-- Fundamental elements
textNode :: String -> Widget Text
textNode content =
  MkWidget do
    doc <- lift $ document =<< window
    t <- lift $ createTextNode content (toDocument doc)
    parent <- ask
    _ <- lift $ appendChild (Text.toNode t) parent
    pure t

elementNode :: forall a. String -> Widget a -> Widget { output :: a, node :: Element }
elementNode tag (MkWidget content) =
  MkWidget do
    doc <- lift $ document =<< window
    node <- lift $ createElement tag (toDocument doc)
    parent <- ask
    _ <- lift $ appendChild (Element.toNode node) parent
    output <- local (const $ Element.toNode node) content
    pure { output, node }

fixWidget :: forall i o. (FE.Event i -> Widget { input :: FE.Event i, output :: FE.Event o }) -> Widget (FE.Event o)
fixWidget f =
  MkWidget
    $ do
        { event, push } <- lift FE.create
        { input, output } <- unWidget $ f event
        _ <- lift $ subscribe input push
        -- TODO Unsubscribe.
        pure output

-- Combinators
text :: String -> Widget Unit
text content = const unit <$> textNode content

element :: forall a. String -> Widget a -> Widget a
element tag content = _.output <$> elementNode tag content

divElement :: forall a. Widget a -> Widget a
divElement = element "div"

inputElement :: Widget HTMLInputElement
inputElement =
  MkWidget
    $ do
        { node } <- unWidget $ elementNode "input" $ pure unit
        pure $ unsafePartial $ fromJust $ fromElement node

extractEvent :: forall a. EventType -> EventTarget -> (Web.Event.Event -> Effect a) -> Widget (FE.Event a)
extractEvent evType inptElem processEvent =
  MkWidget
    $ do
        { event, push } <- lift FE.create
        listener <-
          lift $ eventListener (\evt -> processEvent evt >>= push)
        _ <- lift $ addEventListener (EventType "input") listener false inptElem
        pure event

textInputOnInput :: String -> Widget (Dynamic String)
textInputOnInput initial =
  MkWidget
    $ do
        inptElem <- unWidget inputElement
        lift $ HTMLInputElement.setValue initial inptElem
        event <- unWidget $ extractEvent (EventType "input") (HTMLInputElement.toEventTarget inptElem) (const $ HTMLInputElement.value inptElem)
        pure $ stepDyn initial (event)

buttonElement :: String -> Widget HTMLButtonElement
buttonElement buttonText = do
  { node } <- elementNode "button" $ text buttonText
  pure $ unsafePartial $ fromJust $ HTMLButtonElement.fromElement node

buttonOnClick :: String -> Widget (FE.Event Unit)
buttonOnClick buttonText = do
  btn <- buttonElement buttonText
  extractEvent (EventType "click") (HTMLButtonElement.toEventTarget btn) (const $ pure unit)

-- Execution / backend stuff
runWidgetInBody :: Widget Unit -> Effect Unit
runWidgetInBody w = do
  b <- window >>= document >>= body
  case b of
    Just bb -> runWidgetInNode (HTMLElement.toNode bb) w
    Nothing -> error "<body> tag not found."

runWidgetInNode :: Node -> Widget Unit -> Effect Unit
runWidgetInNode n (MkWidget w) = runReaderT w n
