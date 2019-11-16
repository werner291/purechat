module Purechat.CustomWidgets (showAvatarOrDefault) where

import Prelude

import API.Media (mxcUrlToThumbnailHttpUrl)
import Affjax (URL)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Purechat.Types (SessionInfo)
import Specular.Dom.Builder.Class (elAttr)
import Specular.Dom.Widget (class MonadWidget)

showAvatarOrDefault :: forall m. MonadWidget m => SessionInfo -> Maybe URL -> m Unit
showAvatarOrDefault si url = do
  elAttr "img"
    ( Object.fromFoldable
        [ Tuple "src" (fromMaybe "/static/unknown.png" (mxcUrlToThumbnailHttpUrl si 96 96 <$> url))
        , Tuple "class" "avatar"
        ]
    )
    (pure unit)

