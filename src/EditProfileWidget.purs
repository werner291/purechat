module Purechat.EditProfileWidget (editProfileWidget) where

import API.Media (mxcUrlToHttpUrl)
import Affjax (URL)
import Control.Alt (alt)
import CustomCombinators (affButtonLoop, elClass, pulseSpinner)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Exception (message)
import Foreign.Object as Object
import Prelude (Unit, bind, const, discard, pure, unit, ($), (<$>), (<>), (>>=))
import Purechat.CustomWidgets (uploadingFilePicker)
import Purechat.GlobalEnv (GlobalEnv)
import Purechat.Types (SessionInfo, UserProfile, unUserId)
import Specular.Dom.Builder.Class (el, elAttr, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (Dynamic, dynamic_, never, tagDyn)
import Specular.FRP.Async (RequestState(..))

showAvatarOrDefault :: forall m. MonadWidget m => SessionInfo -> Maybe URL -> m Unit
showAvatarOrDefault si url = do
  elAttr "img"
    ( Object.fromFoldable
        [ Tuple "src" (mxcUrlToHttpUrl si $ fromMaybe "/static/unknown.png" url)
        , Tuple "class" "avatar"
        ]
    )
    (pure unit)



editProfileWidget :: forall m. MonadWidget m => GlobalEnv m -> UserProfile -> m Unit
editProfileWidget env p =
  elClass "div" "profile-edit"
    $ do
        el "h2" $ text "Profile"

        displayname :: Dynamic String <- elClass "div" "input-group" do
          el "h3" $ text "Display Name"
          text "This will not affect your User ID."
          textInputOnInput (fromMaybe (unUserId env.session.user_id) (p.displayname)) Object.empty

        el "h3" $ text "Avatar"

        av_url :: Dynamic (Maybe URL) <- uploadingFilePicker env.session

        dynamic_ $ showAvatarOrDefault env.session <$> (alt p.avatar_url <$> av_url)

        let
          candidateProfile :: Dynamic UserProfile
          candidateProfile = do
            d <- displayname
            a <- av_url
            pure { displayname: Just d, avatar_url: a }
        _ <-
          affButtonLoop
            $ case _ of
                NotRequested -> do
                  clicks <- buttonOnClick (pure $ Object.singleton "class" "save") $ text "Save"
                  pure $ (\pp -> env.updateProfile pp >>= (const $ pure pp)) <$> tagDyn candidateProfile clicks
                Loading -> do
                  pulseSpinner
                  pure never
                Loaded (Left err) -> do
                  text $ "Error occurred: " <> (message err)
                  clicks <- buttonOnClick (pure $ Object.singleton "class" "save") $ text "Save"
                  pure $ (\pp -> env.updateProfile pp >>= (const $ pure pp)) <$> tagDyn candidateProfile clicks
                Loaded (Right mxc) -> do
                  text $ "Profile updated successfully."
                  clicks <- buttonOnClick (pure $ Object.singleton "class" "save") $ text "Save"
                  pure $ (\pp -> env.updateProfile pp >>= (const $ pure pp)) <$> tagDyn candidateProfile clicks
        pure unit
