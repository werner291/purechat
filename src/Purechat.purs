
module Purechat.Purechat (primaryView) where

import Prelude

import API (UserProfile)
import API as API
import CustomCombinators (dynamicMaybe_, elClass)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Purechat.ChannelDirectoryWidget (channelDirectory)
import Purechat.ServerFeed (serverState)
import Purechat.Types (RoomData, RoomId, SessionInfo)
import RoomWidget (roomView)
import Specular.Dom.Builder.Class (elAttr, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (class MonadFRP, Dynamic, Event, dynamic_, holdDyn, leftmost)
import Specular.FRP.Async (RequestState(..), asyncRequest)
import Web.File.Blob (Blob)
import Web.File.Url (createObjectURL)
import Purechat.Widgets.CreateRoomWidget (createRoomWidget)
  
profileBar :: forall m. MonadWidget m => SessionInfo -> m Unit
profileBar si = elClass "div" "profile-bar" do
  profile :: Dynamic (RequestState UserProfile) <- asyncRequest $ pure (API.getProfile si si.user_id)
  dynamic_ $ profile <#> case _ of
    Loaded prof -> do
      
      let 
        -- Watch out: this URL can still contain mxc:// requests that we can't handle directly!
        mkAvatarRequest :: Aff Blob
        mkAvatarRequest = API.getMediaWithPossibleMXC si $ fromMaybe "/static/unknown.png" $ prof.avatar_url 
      avatarBlob :: Dynamic (RequestState Blob) <- asyncRequest $ pure mkAvatarRequest
      dynamic_ $ avatarBlob <#> case _ of
        Loaded a -> do
          av_url <- liftEffect $ createObjectURL a
          elAttr "img" (Object.fromFoldable [Tuple "src" av_url, Tuple "class" "avatar"]) (pure unit)
        _ -> text "Loading avatar."

      elClass "p" "username" $ text $ prof.displayname

      pure unit
    _ -> text $ "Fetching profile..."

sidebar :: forall m. MonadWidget m => SessionInfo -> Dynamic (Map RoomId RoomData) -> m { channelPicked :: Event (RoomId), createRoom :: Event Unit }
sidebar si joined_channels = 
  elClass "div" "sidebar" do
    profileBar si

    createRoom <- buttonOnClick (pure Object.empty) do
      elClass "i" "fas fa-plus" $ pure unit
      text "Create room"

    channelPicked <- channelDirectory joined_channels
    pure {channelPicked,createRoom}

data RoomViewState = 
  PickRoom
  | ShowRoom RoomId
  | CreateRoom

-- The "primary" widget that is visible once the user is logged in .
primaryView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> m Unit
primaryView si = do
    -- -- Keep a list of channels the user has currently joined
    joined_channels_maybe :: Dynamic (Maybe (Map RoomId RoomData)) <- serverState si

    dynamicMaybe_ joined_channels_maybe $ (\joined_channels -> do
      
      {channelPicked, createRoom} <- sidebar si joined_channels

      -- TODO : Make some kind of multi-window view. Should be easy enough by
      --        just making multiple sub-components here...
      currentRoomView <- holdDyn PickRoom $ leftmost [ShowRoom <$> channelPicked, const CreateRoom <$> createRoom]

      dynamic_ $ currentRoomView <#> case _ of
        CreateRoom -> createRoomWidget si >>= const (pure unit)
        ShowRoom rid -> roomView si rid (Map.lookup rid <$> joined_channels)
        PickRoom -> text "Welcome! Please select a room to get started.")

    dynamic_ $ joined_channels_maybe <#> \jcm -> case jcm of
      Just _ -> pure unit
      Nothing -> text "Loading channels, please stand by..."

    pure unit