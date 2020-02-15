module Purechat.ChannelDirectoryWidget (channelDirectory) where

import Prelude

import CustomCombinators (elClass, elemOnClick, pulseSpinner)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Set as Set
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Purechat.ServerFeed (KnownServerState)
import Purechat.Types (RemoteResourceView(..), RoomId, mkRoomId, unRoomId)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (textInput, textInputValueEventOnEnter)
import Specular.FRP (Dynamic, Event, dynamic_, never, subscribeEvent_, withDynamic_)

searchBar :: forall m. MonadWidget m => m (Event String)
searchBar = do
  inpt <-
    textInput
      { initialValue: ""
      , attributes: pure (Object.fromFoldable [ Tuple "placeholder" "Room ID or Alias" ])
      , setValue: never
      }
  textInputValueEventOnEnter inpt

roomLinkLi :: forall env m. MonadWidget m => { openRoom :: RoomId -> Effect Unit | env } -> RoomId -> String -> m Unit
roomLinkLi env rId display_name = do
  clicks :: Event Unit <- elemOnClick "li" mempty $ text display_name
  subscribeEvent_ (const $ env.openRoom rId) clicks

-- dynList :: forall k v. Dynamic (Map k a) -> 

-- A widget showing a short, compact list of all channels the user might currently be interested in.
-- Returns an Event stream of room IDs
channelDirectory :: forall env m.
  MonadWidget m =>
  { openRoom :: RoomId -> Effect Unit | env }
  -> Dynamic (RemoteResourceView (KnownServerState m)) -> m Unit
channelDirectory env rkss = do
  directEnterName :: Event String <- searchBar
  subscribeEvent_ env.openRoom (map mkRoomId directEnterName)
  elClass "div" "channel-directory" do
    withDynamic_ rkss $ case _ of
      RRLoading -> do
        pulseSpinner
        text "Loading Channels..."
      RRLoaded st -> do
      
        el "h2" $ text "Joined rooms"

        withDynamic_ st.rooms_by_tag $ traverseWithIndex_ $ \tag rIds -> do
          text tag
          for_ rIds $ \rId -> do
            withDynamic_ st.joined_rooms $ \rooms -> do
              for_ (Map.lookup rId rooms) $ \jr -> 
                dynamic_ $ jr.meta <#> \meta -> roomLinkLi env rId meta.display_name

        el "ul" $
          withDynamic_ st.joined_rooms $ traverseWithIndex_ (\rId jr -> dynamic_ $ jr.meta <#> \meta -> roomLinkLi env rId meta.display_name)

        el "h2" $ text "Invitations"

        dynamic_ $ st.invited_to <#> \s -> 
          when (Set.isEmpty s) $ text "You have no invitations."

        el "ul" $
          withDynamic_ st.invited_to $ traverse_ (\rId -> roomLinkLi env rId (unRoomId rId))


