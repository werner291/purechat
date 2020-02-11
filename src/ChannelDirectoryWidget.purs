module Purechat.ChannelDirectoryWidget (channelDirectory) where

import Prelude

import API.Rooms (RoomMeta)
import CustomCombinators (elClass, elemOnClick, pulseSpinner, remoteLoadingView)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Purechat.ServerFeed (KnownServerState)
import Purechat.Types (RoomId, mkRoomId, unRoomId, RemoteResourceView)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (textInput, textInputValueEventOnEnter)
import Specular.FRP (Dynamic, Event, dynamic, dynamic_, leftmost, never, subscribeEvent_, switch)
import Specular.FRP.List (dynamicList)

searchBar :: forall m. MonadWidget m => m (Event String)
searchBar = do
  inpt <-
    textInput
      { initialValue: ""
      , attributes: pure (Object.fromFoldable [ Tuple "placeholder" "Room ID or Alias" ])
      , setValue: never
      }
  textInputValueEventOnEnter inpt

-- A widget showing a short, compact list of all channels the user might currently be interested in.
-- Returns an Event stream of room IDs
channelDirectory :: forall env m.
  MonadWidget m =>
  { openRoom :: RoomId -> Effect Unit | env }
  -> Dynamic (RemoteResourceView (KnownServerState m)) -> m Unit
channelDirectory env rkss = 
  let 
    loadedView :: KnownServerState m -> m Unit
    loadedView st = do
      
      let
        clickableLi :: Tuple RoomId RoomMeta -> m (Event RoomId)
        clickableLi (Tuple rId {display_name}) = do
          clicks :: Event Unit <- elemOnClick "li" mempty $ text display_name
          pure $ const rId <$> clicks

        viewrow :: Dynamic (Tuple RoomId (Dynamic RoomMeta)) -> m (Event RoomId)
        viewrow d = do
          x <- dynamic $ d <#> \(Tuple rId dyn_meta) -> do
            dynev <- dynamic $ dyn_meta <#> \meta -> clickableLi (Tuple rId meta)
            pure $ switch dynev
          pure $ switch x

        -- TODO get the display name in here somehow
        clickableLiInvite :: RoomId -> m (Event RoomId)
        clickableLiInvite rId = do
          clicks :: Event Unit <- elemOnClick "li" mempty $ text (fromMaybe (unRoomId rId) Nothing)
          pure $ const rId <$> clicks

        inviterow :: Dynamic RoomId -> m (Event RoomId)
        inviterow d = switch <$> dynamic (d <#> clickableLiInvite)

      el "h2" $ text "Invitations"

      dynamic_ $ st.invited_to <#> \s -> 
        when (Set.isEmpty s) $ text "You have no invitations."

      pickedFromInvite <-
        (switch <<< map leftmost)
          <$> el "ul" (dynamicList (Set.toUnfoldable <$> st.invited_to) $ inviterow)

      el "h2" $ text "Joined rooms"

      pickedFromJoined <-
        (switch <<< map leftmost)
          <$> el "ul" (dynamicList (Map.toUnfoldable <$> st.joined_rooms) $ (\d -> viewrow $ d <#> \(Tuple rId dm) -> (Tuple rId dm.meta)))

      subscribeEvent_ env.openRoom $ leftmost [pickedFromJoined, pickedFromInvite ]
  in do
    directEnterName :: Event String <- searchBar
    subscribeEvent_ env.openRoom (map mkRoomId directEnterName)
    elClass "div" "channel-directory" do
      const unit <$> remoteLoadingView rkss (do 
        pulseSpinner
        text "Loading Channels..."
        ) loadedView

