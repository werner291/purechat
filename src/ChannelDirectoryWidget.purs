module Purechat.ChannelDirectoryWidget (channelDirectory) where

import Prelude

import CustomCombinators (RemoteResourceView, elClass, elemOnClick, pulseSpinner, remoteLoadingView)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Purechat.ServerFeed (KnownServerState, RoomMeta)
import Purechat.Types (RoomId, mkRoomId, unRoomId)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (textInput, textInputValueEventOnEnter)
import Specular.FRP (Dynamic, Event, WeakDynamic, dynamic, dynamic_, leftmost, never, switch, switchWeakDyn, unWeakDynamic, weakDynamic, weakDynamic_)
import Specular.FRP.Async (RequestState)
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
channelDirectory :: forall m. MonadWidget m => Dynamic (RemoteResourceView (KnownServerState m)) -> m (Event RoomId)
channelDirectory rkss = remoteLoadingView rkss $ \st ->
  elClass "div" "channel-directory" do
    directEnterName :: Event String <- searchBar
    let
      clickableLi :: Tuple RoomId RoomMeta -> m (Event RoomId)
      clickableLi (Tuple rId {display_name}) = do
        clicks :: Event Unit <- elemOnClick "li" mempty $ text display_name
        pure $ const rId <$> clicks

      viewrow :: WeakDynamic (Tuple RoomId (Dynamic RoomMeta)) -> m (Event RoomId)
      viewrow d = do
        x <- weakDynamic $ d <#> \(Tuple rId dyn_meta) -> do
          dynev <- dynamic $ dyn_meta <#> \meta -> clickableLi (Tuple rId meta)
          pure $ switch dynev
        pure $ switchWeakDyn x
        -- switchWeakDyn <$> weakDynamic $ d <#> \(Tuple rId viewdata) -> do
        --   dynamic_ $ (viewdata $ pure 0) <#> \rd -> clickableLi (Tuple rId rd)
        --   ?wut

      -- TODO get the display name in here somehow
      clickableLiInvite :: RoomId -> m (Event RoomId)
      clickableLiInvite rId = do
        clicks :: Event Unit <- elemOnClick "li" mempty $ text (fromMaybe (unRoomId rId) Nothing)
        pure $ const rId <$> clicks

      inviterow :: WeakDynamic RoomId -> m (Event RoomId)
      inviterow d = switchWeakDyn <$> weakDynamic (d <#> clickableLiInvite)
    el "h2" $ text "Invitations"
    weakDynamic_ $ st.invited_to <#> \s -> 
      if (Set.isEmpty s) 
        then text "You have no invitations."
        else 
    pickedFromInvite <-
      (switchWeakDyn <<< map leftmost)
        <$> el "ul" (weakDynamicList (Set.toUnfoldable <<< _.invited_to <$> st) $ inviterow)
    el "h2" $ text "Joined rooms"
    pickedFromJoined <-
      (switchWeakDyn <<< map leftmost)
        <$> el "ul" (weakDynamicList (Map.toUnfoldable <<< _.joined_rooms <$> st) $ (\d -> viewrow $ d <#> \(Tuple rId dm) -> (Tuple rId dm.meta)))--  viewrow (Tuple rId (dm <#> _.meta)))
    dynamic_ $ (unWeakDynamic st)
      <#> case _ of
          Just _ -> pure unit
          Nothing -> do
            pulseSpinner
            text "Loading channels..."
    pure $ leftmost [ (map mkRoomId directEnterName), pickedFromJoined, pickedFromInvite ]
