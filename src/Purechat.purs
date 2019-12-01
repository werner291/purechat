module Purechat.Purechat (primaryPage) where

import Prelude

import API.Profile (getProfile)
import CustomCombinators (elClass, elemOnClick)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object as Object
import Purechat.Types (RoomId, SessionInfo, UserProfile, unUserId)
import Purechat.Widgets.CreateRoomWidget (createRoomWidget)
import RoomWidget (roomView)
import Turbine (div)

profileBar :: forall o m. SessionInfo -> Behavior (Maybe UserProfile) -> Component { openProfile :: Event Unit } o
profileBar si profile =
  div { class: pure "profile-bar" } dynamic $ profile <#> case _ of
    (Just prof) -> do
      --showAvatarOrDefault si prof.avatar_url
      elClass "p" "username" $ text $ fromMaybe (unUserId si.user_id) prof.displayname
      elemOnClick "i" (Object.singleton "class" "fas fa-cog") $ pure unit
    Nothing -> do
      text $ "Fetching profile..."
      pure never
          

sidebar :: forall m. MonadWidget m => SessionInfo -> WeakDynamic UserProfile -> WeakDynamic KnownServerState -> m { channelPicked :: Event (RoomId), createRoom :: Event Unit, openProfile :: Event Unit }
sidebar si profile st =
  elClass "div" "sidebar" do
    { openProfile } <- profileBar si profile
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
    { server_state :: WeakDynamic KnownServerState
    , current_profile :: WeakDynamic UserProfile
    }
modelStore si profileUpdates = do
  -- Keep a list of channels the user has currently joined
  server_state <- serverState si (pure Map.empty)
  -- Keep the most recent known version of the user's profile information
  profile_from_server_state :: Dynamic (RequestState UserProfile) <- asyncRequest $ pure (getProfile si si.user_id)
  current_profile <- holdWeakDyn $ leftmost [ profileUpdates, filterMapEvent fromLoaded $ changed profile_from_server_state ]
  pure { server_state, current_profile }

interruptedEvent :: forall a. Dynamic (Maybe (Event a)) -> Event a
interruptedEvent d =
  switch $ d
    >>= case _ of
        Just a -> pure a
        Nothing -> pure never

-- The "primary" widget that is visible once the user is logged in .
primaryPage :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m Unit
primaryPage si =
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
                  profs :: WeakDynamic (Event UserProfile) <- weakDynamic $ current_profile <#> \prof -> editProfileWidget si prof
                  pure $ UserProfileOutput (switchWeakDyn profs)
                CreateRoom -> do
                  _ <- createRoomWidget si
                  pure NoOutput
                ShowRoom rid -> do
                  roomView si rid $ unWeakDynamic (_.joined_rooms <$> server_state)
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
