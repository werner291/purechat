module Purechat.EditProfileWidget (editProfileWidget) where

import API (UserProfile)
import API as API
import Affjax (URL)
import CustomCombinators (affButtonLoop, elClass, pulseSpinner)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (message)
import Foreign.Object as Object
import Prelude (Unit, bind, const, discard, identity, map, pure, unit, ($), (<$>), (<<<), (<>), (>>=))
import Purechat.Types (SessionInfo)
import Specular.Dom.Builder.Class (domEventWithSample, el, elAttr, elAttr', text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (class MonadFRP, Dynamic, Event, filterMapEvent, holdDyn, never, tagDyn)
import Specular.FRP.Async (RequestState(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File (File, toBlob)
import Web.File.FileList as FL
import Web.HTML.HTMLInputElement (files)

showAvatarOrDefault :: forall m. MonadWidget m => SessionInfo -> Maybe URL -> m Unit
showAvatarOrDefault si url = do
  elAttr "img"
    ( Object.fromFoldable
        [ Tuple "src" (API.mxcUrlToHttpUrl si $ fromMaybe "/static/unknown.png" url)
        , Tuple "class" "avatar"
        ]
    )
    (pure unit)

fileInputOnChange :: forall m. MonadWidget m => m (Event (Maybe File))
fileInputOnChange = do
  Tuple element _ <- elAttr' "input" (Object.singleton "type" "file") (pure unit)
  let
    getFileHack :: _ -> Effect (Maybe File)
    getFileHack _ = do
      fs <- files (unsafeCoerce element)
      pure $ (fs >>= FL.item 0)
  domChanged <- domEventWithSample getFileHack "input" element
  pure domChanged

editProfileWidget :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> UserProfile -> m (Event UserProfile)
editProfileWidget si p =
  elClass "div" "profile-edit"
    $ do
        text "Display name: "
        displayname :: Dynamic String <- textInputOnInput p.displayname Object.empty
        el "br" $ pure unit
        text "Avatar: "
        av_url_updates :: Event URL <-
          affButtonLoop
            $ case _ of
                NotRequested -> do
                  showAvatarOrDefault si p.avatar_url
                  fileChosen <- fileInputOnChange
                  pure $ (API.uploadMXC si) <<< toBlob <$> filterMapEvent identity fileChosen
                Loading -> do
                  pulseSpinner
                  pure never
                Loaded (Left err) -> do
                  showAvatarOrDefault si p.avatar_url
                  fileChosen <- fileInputOnChange
                  pure $ (API.uploadMXC si) <<< toBlob <$> filterMapEvent identity fileChosen
                Loaded (Right mxc) -> do
                  showAvatarOrDefault si (Just mxc)
                  fileChosen <- fileInputOnChange
                  pure $ (API.uploadMXC si) <<< toBlob <$> filterMapEvent identity fileChosen
        av_url :: Dynamic (Maybe String) <- holdDyn p.avatar_url (map Just av_url_updates)
        let
          candidateProfile :: Dynamic UserProfile
          candidateProfile = do
            d :: String <- displayname :: Dynamic String
            a :: Maybe URL <- av_url
            pure { displayname: d, avatar_url: a }
        updt <-
          affButtonLoop
            $ case _ of
                NotRequested -> do
                  clicks <- buttonOnClick (pure Object.empty) $ text "Save"
                  pure $ (\pp -> API.putProfile si pp >>= (const $ pure pp)) <$> tagDyn candidateProfile clicks
                Loading -> do
                  pulseSpinner
                  pure never
                Loaded (Left err) -> do
                  text $ "Error occurred: " <> (message err)
                  clicks <- buttonOnClick (pure Object.empty) $ text "Save"
                  pure $ (\pp -> API.putProfile si pp >>= (const $ pure pp)) <$> tagDyn candidateProfile clicks
                Loaded (Right mxc) -> do
                  text $ "Profile updated successfully."
                  clicks <- buttonOnClick (pure Object.empty) $ text "Save"
                  pure $ (\pp -> API.putProfile si pp >>= (const $ pure pp)) <$> tagDyn candidateProfile clicks
        pure updt
