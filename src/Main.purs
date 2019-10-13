module Main where

import Prelude

import Data.Foldable (for_)
import Data.String as S
import Effect (Effect)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Input (textInput, textInputValue)
import Specular.FRP (dynamic_, never)
import Specular.FRP.List (dynamicListWithIndex_)

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = do

  ti <- textInput { initialValue : "Hello world!", setValue : never, attributes : pure mempty }

  let the_string = textInputValue ti

  dynamicListWithIndex_ (map (map S.singleton <<< S.toCodePointArray) the_string) 
    (\idx strDyn -> dynamic_ $ text<$>strDyn )

  pure unit

main :: Effect Unit
main = runMainWidgetInBody mainWidget