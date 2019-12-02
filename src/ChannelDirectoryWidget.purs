module Purechat.ChannelDirectoryWidget (channelDirectory) where

import Prelude

import CustomCombinators (RemoteResourceView, elClass, elemOnClick, remoteLoadingView)
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
import Specular.FRP (Dynamic, Event, dynamic, dynamic_, leftmost, never, switch)
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
channelDirectory :: forall m. MonadWidget m => Dynamic (RemoteResourceView (KnownServerState m)) -> m (Event RoomId)
channelDirectory rkss = 
  let 
    loadedView :: KnownServerState m -> m (Event RoomId)
    loadedView st = elClass "div" "channel-directory" do
      directEnterName :: Event String <- searchBar
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
          -- switchWeakDyn <$> weakDynamic $ d <#> \(Tuple rId viewdata) -> do
          --   dynamic_ $ (viewdata $ pure 0) <#> \rd -> clickableLi (Tuple rId rd)
          --   ?wut

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
      
      pure $ leftmost [ (map mkRoomId directEnterName), pickedFromJoined, pickedFromInvite ]
  in do 
    evm :: Dynamic (Maybe (Event RoomId)) <- remoteLoadingView rkss loadedView (pure unit)
    pure $ switch (evm <#> fromMaybe never)

