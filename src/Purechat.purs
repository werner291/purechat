module Purechat.Purechat (primaryView) where

import API (UserProfile)
import API as API
import CustomCombinators (dynamicMaybe, elClass, elemOnClick)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Prelude (Unit, bind, const, discard, map, pure, unit, ($), (<#>), (<$>), (>>=))
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.CustomWidgets (showAvatarOrDefault)
import Purechat.EditProfileWidget (editProfileWidget)
import Purechat.ServerFeed (serverState)
import Purechat.Types (RoomData, RoomId, SessionInfo)
import Purechat.Widgets.CreateRoomWidget (createRoomWidget)
import RoomWidget (roomView)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (class MonadFRP, Dynamic, Event, dynamic, dynamic_, fixFRP_, holdDyn, holdWeakDyn, leftmost, never, switch, unWeakDynamic)
import Specular.FRP.Async (RequestState, asyncRequest, fromLoaded)



profileBar :: forall m. MonadWidget m => SessionInfo -> Dynamic (Maybe UserProfile) -> m { openProfile :: Event Unit }
profileBar si profile =
  elClass "div" "profile-bar" do
    openProfile :: Event Unit <-
      switch
        <$> ( dynamic $ profile
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

sidebar :: forall m. MonadWidget m => SessionInfo -> Dynamic (Maybe UserProfile) -> Dynamic (Map RoomId RoomData) -> m { channelPicked :: Event (RoomId), createRoom :: Event Unit, openProfile :: Event Unit }
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
    { joined_channels_maybe :: Dynamic (Maybe (Map RoomId RoomData))
    , current_profile :: Dynamic (Maybe UserProfile)
    }
modelStore si profileUpdates = do
  -- Keep a list of channels the user has currently joined
  joined_channels_maybe :: Dynamic (Maybe (Map RoomId RoomData)) <- serverState si
  -- Keep the most recent known version of the user's profile information
  profile_from_server_state :: Dynamic (RequestState UserProfile) <- asyncRequest $ pure (API.getProfile si si.user_id)
  self_profile_state <- unWeakDynamic <$> holdWeakDyn profileUpdates
  let
    current_profile =
      self_profile_state
        >>= case _ of
            Just s -> self_profile_state
            Nothing -> map fromLoaded profile_from_server_state
  pure { joined_channels_maybe, current_profile }

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
        { joined_channels_maybe, current_profile } <- modelStore si evt
        let
          flattenOutput :: Dynamic (Maybe (Dynamic OutputCases)) -> Dynamic OutputCases
          flattenOutput d =
            d >>= case _ of
                  Just a -> a
                  Nothing -> pure NoOutput
        outpt :: Dynamic OutputCases <-
          map flattenOutput $ dynamicMaybe joined_channels_maybe
            $ ( \joined_channels -> do
                  { channelPicked, createRoom, openProfile } <- sidebar si current_profile joined_channels
                  -- TODO : Make some kind of multi-window view. Should be easy enough by
                  --        just making multiple sub-components here...
                  currentRoomView <- holdDyn PickRoom $ leftmost [ ShowRoom <$> channelPicked, const CreateRoom <$> createRoom, const EditProfile <$> openProfile ]
                  dynamic $ currentRoomView
                    <#> case _ of
                        EditProfile -> do
                          maybeProf :: Dynamic (Maybe (Event UserProfile)) <-
                            dynamicMaybe current_profile
                              $ \dprof -> do
                                  profOut :: Dynamic (Event UserProfile) <- dynamic $ dprof <#> \prof -> editProfileWidget si prof
                                  pure $ switch profOut
                          pure $ UserProfileOutput
                            $ switch
                                ( maybeProf
                                    <#> case _ of
                                        Just e -> e
                                        Nothing -> never
                                )
                        CreateRoom -> do
                          _ <- createRoomWidget si
                          pure NoOutput
                        ShowRoom rid -> do
                          roomView si rid (Map.lookup rid <$> joined_channels)
                          pure NoOutput
                        PickRoom -> do
                          text "Welcome! Please select a room to get started."
                          pure NoOutput
              )
        let
          profile_updates :: Event UserProfile
          profile_updates =
            switch
              ( outpt
                  <#> case _ of
                      (UserProfileOutput ev) -> ev
                      _ -> never
              )
        dynamic_ $ joined_channels_maybe
          <#> \jcm -> case jcm of
              Just _ -> pure unit
              Nothing -> text "Loading channels, please stand by..."
        pure profile_updates
