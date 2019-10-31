module Purechat.ChannelDirectoryWidget (channelDirectory) where

import Prelude

import CustomCombinators (elClass, elemOnClick)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Purechat.ServerFeed (KnownServerState)
import Purechat.Types (RoomData, RoomId, mkRoomId, unRoomId)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (textInput, textInputValueEventOnEnter)
import Specular.FRP (Event, WeakDynamic, leftmost, never, switchWeakDyn, weakDynamic, weakDynamic_)
import Specular.FRP.List (weakDynamicList)

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
channelDirectory :: forall m. MonadWidget m => WeakDynamic KnownServerState -> m (Event RoomId)
channelDirectory st =
  elClass "div" "channel-directory" do
    directEnterName :: Event String <- searchBar
    let
      clickableLi :: (Tuple RoomId RoomData) -> m (Event RoomId)
      clickableLi (Tuple rId rd) = do
        clicks :: Event Unit <- elemOnClick "li" mempty $ text (fromMaybe (unRoomId rId) rd.state.display_name)
        pure $ const rId <$> clicks

      viewrow :: WeakDynamic (Tuple RoomId RoomData) -> m (Event RoomId)
      viewrow d = switchWeakDyn <$> weakDynamic (d <#> clickableLi)

      -- TODO get the display name in here somehow
      clickableLiInvite :: RoomId -> m (Event RoomId)
      clickableLiInvite rId = do
        clicks :: Event Unit <- elemOnClick "li" mempty $ text (fromMaybe (unRoomId rId) Nothing)
        pure $ const rId <$> clicks

      inviterow :: WeakDynamic RoomId -> m (Event RoomId)
      inviterow d = switchWeakDyn <$> weakDynamic (d <#> clickableLiInvite)

    el "h2" $ text "Invitations"
    
    weakDynamic_ $ st <#> \s -> when (Set.isEmpty s.invited_to) $ text "You have no invitations."

    pickedFromInvite <- (switchWeakDyn <<< map leftmost) <$> el "ul" (weakDynamicList (Set.toUnfoldable <<< _.invited_to <$> st) $ inviterow)

    el "h2" $ text "Joined rooms"
    pickedFromJoined <- (switchWeakDyn <<< map leftmost) <$> el "ul" (weakDynamicList (Map.toUnfoldable <<< _.joined_rooms <$> st) $ viewrow)
    
    pure $ leftmost [ (map mkRoomId directEnterName), pickedFromJoined, pickedFromInvite ]
