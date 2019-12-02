module Purechat.Purechat (primaryView) where

import Prelude
import API.Profile (getProfile)
import CustomCombinators (RemoteResourceView, bridgeEventOverMaybe, elClass, elemOnClick, pulseSpinner, remoteLoadingView, toLoadedUpdates)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object as Object
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.EditProfileWidget (editProfileWidget)
import Purechat.ServerFeed (KnownServerState, serverState)
import Purechat.Types (RoomId, SessionInfo, UserProfile, unUserId)
import Purechat.Widgets.CreateRoomWidget (createRoomWidget)
import RoomWidget (roomView)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (class MonadFRP, Dynamic, Event, changed, dynamic, filterMapEvent, fixFRP_, holdDyn, leftmost, never, switch)
import Specular.FRP.Async (RequestState, asyncRequest, fromLoaded)

profileBar :: forall m. MonadWidget m => SessionInfo -> UserProfile -> m { openProfile :: Event Unit }
profileBar si profile =
  elClass "div" "profile-bar" do
    showAvatarOrDefault si profile.avatar_url
    elClass "p" "username" $ text $ fromMaybe (unUserId si.user_id) profile.displayname
    openProfile :: Event Unit <- elemOnClick "i" (Object.singleton "class" "fas fa-cog") $ pure unit
    pure { openProfile }

bridgeProfile :: Dynamic (Maybe { openProfile :: Event Unit }) -> { openProfile :: Event Unit }
bridgeProfile d = { openProfile: bridgeEventOverMaybe $ map (map _.openProfile) d }

sidebar ::
  forall m.
  MonadWidget m =>
  SessionInfo ->
  Dynamic (RemoteResourceView UserProfile) ->
  Dynamic (RemoteResourceView (KnownServerState m)) ->
  m { channelPicked :: Event (RoomId), createRoom :: Event Unit, openProfile :: Event Unit }
sidebar si profile st =
  elClass "div" "sidebar" do
    { openProfile } <- bridgeProfile <$> (remoteLoadingView profile pulseSpinner (profileBar si))
    createRoom <-
      buttonOnClick (pure Object.empty) do
        elClass "i" "fas fa-plus" $ pure unit
        text "Create room"
    channelPicked <- channelDirectory st
    pure { channelPicked, createRoom, openProfile }

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
primaryView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m Unit
primaryView si =
  fixFRP_
    $ \(evt :: Event UserProfile) -> do
        { server_state, current_profile } <- modelStore si evt
        { channelPicked, createRoom, openProfile } <- sidebar si current_profile server_state
        -- TODO : Make some kind of multi-window view. Should be easy enough by
        --        just making multiple sub-components here...
        currentRoomView <- holdDyn PickRoom $ leftmost [ ShowRoom <$> channelPicked, const CreateRoom <$> createRoom, const EditProfile <$> openProfile ]
        outpt :: Dynamic OutputCases <-
          elClass "div" "main-view" $ dynamic $ currentRoomView
            <#> case _ of
                EditProfile -> do
                  profs <- bridgeEventOverMaybe <$> remoteLoadingView current_profile pulseSpinner (editProfileWidget si)
                  pure $ UserProfileOutput profs
                CreateRoom -> do
                  _ <- createRoomWidget si
                  pure NoOutput
                ShowRoom rid -> do
                  _ <- remoteLoadingView server_state pulseSpinner $ \st -> roomView si rid $ (Map.lookup rid <$> st.joined_rooms)
                  pure NoOutput
                PickRoom -> do
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
