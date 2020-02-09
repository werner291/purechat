module Purechat.Purechat (primaryView) where

import Prelude

import API.Profile (getProfile)
import CustomCombinators (RemoteResourceView, bridgeEventOverMaybe, dynamicMaybe_, elClass, elemOnClick, pulseSpinner, remoteLoadingView, remoteLoadingView_, toLoadedUpdates)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.EditProfileWidget (editProfileWidget)
import Purechat.ServerFeed (KnownServerState, serverState)
import Purechat.Types (GlobalEnv, RoomId, SessionInfo, UserId, UserProfile, unUserId)
import Purechat.Widgets.CreateRoomWidget (createRoomWidget)
import Purechat.Widgets.ProfileCard (profileCard)
import Record as Record
import RoomWidget (roomView)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (class MonadFRP, Dynamic, Event, changed, dynamic, filterMapEvent, fixFRP_, leftmost, never, newDynamic, readDynamic, subscribeEvent_, switch)
import Specular.FRP.Async (RequestState, asyncRequest, fromLoaded)

profileBar ::
  forall m.
  MonadWidget m =>
  GlobalEnv ->
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
  GlobalEnv ->
  Dynamic (RemoteResourceView UserProfile) ->
  Dynamic (RemoteResourceView (KnownServerState m)) ->
  m Unit
sidebar env profile st =
  elClass "div" "sidebar" do
    remoteLoadingView_ profile pulseSpinner (profileBar env)
    clicks <- buttonOnClick (pure Object.empty) $ text "Create room"
    subscribeEvent_ (\_ -> env.createRoom) clicks
    -- subscribeEvent_ (const ) =<< ()
    --     elClass "i" "fas fa-plus" $ pure unit
    channelDirectory env st

data RoomViewState
  = PickRoom
  | ShowRoom RoomId
  | CreateRoom
  | EditProfile

data OutputCases
  = UserProfileOutput (Event UserProfile)
  | NoOutput

modelStore ::
  forall m.
  MonadFRP m =>
  SessionInfo ->
  Event UserProfile ->
  m
    { server_state :: Dynamic (RemoteResourceView (KnownServerState m))
    , current_profile :: Dynamic (RemoteResourceView UserProfile)
    }
modelStore si profileUpdates = do
  -- Keep a list of channels the user has currently joined
  server_state <- serverState si
  -- Keep the most recent known version of the user's profile information
  profile_from_server_state :: Dynamic (RequestState UserProfile) <- asyncRequest $ pure (getProfile si si.user_id)
  current_profile <- toLoadedUpdates $ leftmost [ profileUpdates, filterMapEvent fromLoaded $ changed profile_from_server_state ]
  pure { server_state, current_profile }

interruptedEvent :: forall a. Dynamic (Maybe (Event a)) -> Event a
interruptedEvent d =
  switch $ d
    >>= case _ of
        Just a -> pure a
        Nothing -> pure never

-- The "primary" widget that is visible once the user is logged in .
primaryView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> { logout :: Effect Unit } -> m Unit
primaryView si env = do
  currentRoomView <- newDynamic PickRoom
  currentProfileCard <- newDynamic Nothing
  let
    inner_env :: GlobalEnv
    inner_env =
      Record.merge env
        { createRoom: currentRoomView.set CreateRoom
        , editProfile: currentRoomView.set EditProfile
        , openRoom: \rId -> currentRoomView.set (ShowRoom rId)
        , logout: env.logout
        , session: si
        , showProfile: \userId profile -> currentProfileCard.set (Just { userId, profile })
        , closeCurrentProfileCard: currentProfileCard.set Nothing
        }
  dynamicMaybe_ currentProfileCard.dynamic (profileCard inner_env)
  elClass "div" "main-container" $ fixFRP_
    $ \(evt :: Event UserProfile) -> do
        { server_state, current_profile } <- modelStore si evt
        -- { channelPicked, createRoom, openProfile } <- 
        sidebar inner_env current_profile server_state
        -- current_profile server_state
        -- TODO : Make some kind of multi-window view. Should be easy enough by
        --        just making multiple sub-components here...
        -- currentRoomView <- holdDyn PickRoom $ leftmost [ ShowRoom <$> channelPicked, const CreateRoom <$> createRoom, const EditProfile <$> openProfile ]
        outpt :: Dynamic OutputCases <-
          elClass "div" "main-view" $ dynamic $ currentRoomView.dynamic
            <#> case _ of
                EditProfile -> do
                  profs <- bridgeEventOverMaybe <$> remoteLoadingView current_profile pulseSpinner (editProfileWidget si)
                  pure $ UserProfileOutput profs
                CreateRoom -> do
                  _ <- createRoomWidget si
                  pure NoOutput
                ShowRoom rid -> do
                  _ <-
                    remoteLoadingView server_state pulseSpinner
                      $ \st -> do
                          -- Note: We specifically take a snapshot of the joined_rooms here!
                          -- We cannot simply map as that would cause the dynamic to update when unrelated rooms change.
                          -- Perhaps this shows that `Dynamic (Map k v)` isn't quite the right idea here.
                          -- Maybe some structure where you can subscribe to updates/insertions/deletions,
                          -- similar to how fanOutM works?
                          roomD <- readDynamic (Map.lookup rid <$> st.joined_rooms)
                          -- Yuck, the `pure` here should be a Dynamic that's dedicated to that room.
                          roomView inner_env rid (pure roomD)
                  pure NoOutput
                PickRoom -> do
                  elClass "div" "no-room-selected" do
                    text "Welcome! Please select a room to get started."
                  pure NoOutput
        let
          profile_updates :: Event UserProfile
          profile_updates =
            switch
              ( outpt
                  <#> case _ of
                      (UserProfileOutput ev) -> ev
                      _ -> never
              )
        pure profile_updates
