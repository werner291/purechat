module Purechat.Widgets.ProfileCard where

import Prelude

import CustomCombinators (elClass, elemOnClick)
import Data.Maybe (Maybe(..))
import Effect.Class.Console as Console
import Foreign.Object as Object
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.GlobalEnv (GlobalEnv)
import Purechat.Types (UserId, UserProfile, unUserId)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (subscribeEvent_)

profileCard ::
  forall m.
  MonadWidget m =>
  GlobalEnv m ->
  { userId :: UserId, profile :: Maybe UserProfile } ->
  m Unit
profileCard env toShow =
  elClass "div" "profile-card" do
    close <- elemOnClick "i" (Object.singleton "class" "fas fa-times close-profile-card") $ pure unit
    subscribeEvent_ (const env.closeCurrentProfileCard) close

    elClass "p" "username" $ text $ unUserId toShow.userId
    case toShow.profile of
      Just prof -> do
        case prof.displayname of
          Just dn -> elClass "p" "displayname" $ text $ "AKA:" <> dn
          _ -> pure unit
        showAvatarOrDefault env.session prof.avatar_url
      _ -> pure unit
    
