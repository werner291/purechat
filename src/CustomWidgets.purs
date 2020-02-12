module Purechat.CustomWidgets where

import Prelude

import API.Media (mxcUrlToThumbnailHttpUrl, uploadMXC)
import Affjax (URL)
import CustomCombinators (affButtonLoop, pulseSpinner)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Foreign.Object as Object
import Purechat.Types (SessionInfo)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (domEventWithSample, elAttr, elAttr')
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (Dynamic, Event, filterMapEvent, holdDyn, never)
import Specular.FRP.Async (RequestState(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File (File, toBlob)
import Web.File.FileList as FL
import Web.HTML.HTMLInputElement (files)

showAvatarOrDefault :: forall m. MonadWidget m => SessionInfo -> Maybe URL -> m Unit
showAvatarOrDefault si url = do
  elAttr "img"
    ( Object.fromFoldable
        [ Tuple "src" (fromMaybe "/static/unknown.png" (mxcUrlToThumbnailHttpUrl si 96 96 <$> url))
        , Tuple "class" "avatar"
        ]
    )
    (pure unit)

fileInputOnChange' :: forall m. MonadWidget m => m (Tuple Node (Event (Maybe File)))
fileInputOnChange' = do
  Tuple element _ <- elAttr' "input" (Object.singleton "type" "file") (pure unit)
  let
    getFileHack :: _ -> Effect (Maybe File)
    getFileHack _ = do
      fs <- files (unsafeCoerce element)
      pure $ (fs >>= FL.item 0)
  domChanged <- domEventWithSample getFileHack "input" element
  pure (Tuple element domChanged)

fileInputOnChange :: forall m. MonadWidget m => m (Event (Maybe File))
fileInputOnChange = Tuple.snd <$> fileInputOnChange'

uploadingFilePicker :: forall m. MonadWidget m => SessionInfo -> m (Dynamic (Maybe URL))
uploadingFilePicker session = do
  av_url_updates <- affButtonLoop
    $ case _ of
        NotRequested -> do
          fileChosen <- fileInputOnChange
          pure $ (uploadMXC session) <<< toBlob <$> filterMapEvent identity fileChosen
        Loading -> do
          pulseSpinner
          pure never
        Loaded (Left err) -> do
          fileChosen <- fileInputOnChange
          pure $ (uploadMXC session) <<< toBlob <$> filterMapEvent identity fileChosen
        Loaded (Right mxc) -> do
          fileChosen <- fileInputOnChange
          pure $ (uploadMXC session) <<< toBlob <$> filterMapEvent identity fileChosen

  holdDyn Nothing (map Just av_url_updates)