module Purechat.Purechat (primaryView) where

import Prelude

import API.Profile (getProfile)
import CustomCombinators (elClass, elemOnClick)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.EditProfileWidget (editProfileWidget)
import Purechat.ServerFeed (serverState)
import Purechat.Types (RoomData, RoomId, SessionInfo, UserProfile)
import Purechat.Widgets.CreateRoomWidget (createRoomWidget)
import RoomWidget (roomView)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, changed, dynamic, filterMapEvent, fixFRP_, holdDyn, holdWeakDyn, leftmost, never, switch, switchWeakDyn, unWeakDynamic, weakDynamic)
import Specular.FRP.Async (RequestState, asyncRequest, fromLoaded)

profileBar :: forall m. MonadWidget m => SessionInfo -> WeakDynamic UserProfile -> m { openProfile :: Event Unit }
profileBar si profile =
  elClass "div" "profile-bar" do
    openProfile :: Event Unit <-
      switch
        <$> ( dynamic $ (unWeakDynamic profile)
              <#> case _ of
                  (Just prof) -> do
                    showAvatarOrDefault si prof.avatar_url
                    elClass "p" "username" $ text $ prof.displayname
                    elemOnClick "i" (Object.singleton "class" "fas fa-cog") $ pure unit
                  Nothing -> do
                    text $ "Fetching profile..."
                    pure never
          )
    pure { openProfile: openProfile }

sidebar :: forall m. MonadWidget m => SessionInfo -> WeakDynamic UserProfile -> WeakDynamic (Map RoomId RoomData) -> m { channelPicked :: Event (RoomId), createRoom :: Event Unit, openProfile :: Event Unit }
sidebar si profile joined_channels =
  elClass "div" "sidebar" do
    { openProfile } <- profileBar si profile
    createRoom <-
      buttonOnClick (pure Object.empty) do
        elClass "i" "fas fa-plus" $ pure unit
        text "Create room"
    channelPicked <- channelDirectory joined_channels
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
    { joined_channels :: WeakDynamic (Map RoomId RoomData)
    , current_profile :: WeakDynamic UserProfile
    }
modelStore si profileUpdates = do
  -- Keep a list of channels the user has currently joined
  joined_channels :: WeakDynamic (Map RoomId RoomData) <- serverState si
  -- Keep the most recent known version of the user's profile information
  profile_from_server_state :: Dynamic (RequestState UserProfile) <- asyncRequest $ pure (getProfile si si.user_id)
  current_profile <- holdWeakDyn $ leftmost [ profileUpdates, filterMapEvent fromLoaded $ changed profile_from_server_state ]
  pure { joined_channels, current_profile }

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
        { joined_channels, current_profile } <- modelStore si evt
        { channelPicked, createRoom, openProfile } <- sidebar si current_profile joined_channels
        -- TODO : Make some kind of multi-window view. Should be easy enough by
        --        just making multiple sub-components here...
        currentRoomView <- holdDyn PickRoom $ leftmost [ ShowRoom <$> channelPicked, const CreateRoom <$> createRoom, const EditProfile <$> openProfile ]
        outpt :: Dynamic OutputCases <-
          elClass "div" "main-view" $ dynamic $ currentRoomView
            <#> case _ of
                EditProfile -> do
                  profs :: WeakDynamic (Event UserProfile) <- weakDynamic $ current_profile <#> \prof -> editProfileWidget si prof
                  pure $ UserProfileOutput (switchWeakDyn profs)
                CreateRoom -> do
                  _ <- createRoomWidget si
                  pure NoOutput
                ShowRoom rid -> do
                  roomView si rid $ unWeakDynamic joined_channels
                    >>= case _ of
                        Just c -> pure $ Map.lookup rid c
                        Nothing -> pure Nothing
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
