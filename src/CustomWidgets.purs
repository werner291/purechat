module Purechat.CustomWidgets (showAvatarOrDefault) where

import API as API
import Affjax (URL)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prelude (Unit, pure, unit, ($))
import Purechat.Types (SessionInfo)
import Specular.Dom.Builder.Class (elAttr)
import Specular.Dom.Widget (class MonadWidget)

showAvatarOrDefault :: forall m. MonadWidget m => SessionInfo -> Maybe URL -> m Unit
showAvatarOrDefault si url = do
  elAttr "img"
    ( Object.fromFoldable
        [ Tuple "src" (API.mxcUrlToHttpUrl si $ fromMaybe "/static/unknown.png" url)
        , Tuple "class" "avatar"
        ]
    )
    (pure unit)