module Purechat.Widgets.Sidebar (sidebar) where

import Prelude

import CustomCombinators (elClass, elemOnClick, pulseSpinner)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object as Object
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.GlobalEnv (GlobalEnv)
import Purechat.Types (UserProfile, unUserId)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Event, dynamic_, subscribeEvent_)

profileBar ::
  forall m.
  MonadWidget m =>
  GlobalEnv m ->
  UserProfile -> m Unit
profileBar env profile =
  elClass "div" "profile-bar" do
    showAvatarOrDefault env.session profile.avatar_url
    elClass "p" "username" $ text $ fromMaybe (unUserId env.session.user_id) profile.displayname
    -- openContextMenu <- elemOnClick "span" Object.empty $ text "oo"
    -- contextMenuIsOpen <- foldDyn (\_ isOpen -> not isOpen) false openContextMenu
    openProfile :: Event Unit <- elemOnClick "i" (Object.singleton "class" "fas fa-cog") $ pure unit
    subscribeEvent_ (const env.editProfile) openProfile
    logout :: Event Unit <- elemOnClick "i" (Object.singleton "class" "fas fa-sign-out-alt") $ pure unit
    subscribeEvent_ (const env.logout) logout

sidebar ::
  forall m.
  MonadWidget m =>
  GlobalEnv m ->
  m Unit
sidebar env = 
  elClass "div" "sidebar" do
    dynamic_ $ env.user_profile <#> case _ of
      Nothing -> pulseSpinner
      Just prof -> profileBar env prof
    clicks <- buttonOnClick (pure Object.empty) $ text "Create room"
    subscribeEvent_ (\_ -> env.createRoom) clicks
    -- subscribeEvent_ (const ) =<< ()
    --     elClass "i" "fas fa-plus" $ pure unit
    channelDirectory env env.channels_state