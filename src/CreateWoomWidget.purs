module Purechat.Widgets.CreateRoomWidget where

import Prelude

import API.Rooms (createRoom)
import CustomCombinators (affButtonLoopSimplified, elClass, elemOnClick, pulseSpinner)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff (message)
import Foreign.Object as Object
import Purechat.Types (RoomId, SessionInfo, UserId(..), unUserId)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (Dynamic, Event, dynamic, fixEvent, foldDyn, leftmost, switch, tagDyn)
import Specular.FRP.List (dynamicList)
import CustomCombinators (elemOnClick)

data InviteeListAction
  = AddInvitee UserId
  | RemoveInvitee UserId

inviteesList :: forall m. MonadWidget m => m (Dynamic (Set UserId))
inviteesList =
  fixEvent
    $ \inviteeUpdate -> do
        invitees <-
          foldDyn
            ( case _ of
                AddInvitee uid -> Set.insert uid
                RemoveInvitee uid -> Set.delete uid
            )
            Set.empty
            inviteeUpdate
        removals <-
          elClass "ul" "invitees" do
            d <-
              dynamicList (Set.toUnfoldable <$> invitees)
                $ \duid ->
                    map switch $ dynamic $ duid
                      <#> ( \uid ->
                            elClass "li" "invitee" do
                              text $ unUserId uid
                              r <- elemOnClick "i" (Object.singleton "class" "invitee-remove fas fa-times") (pure unit)
                              pure $ const (RemoveInvitee uid) <$> r
                        )
            pure $ switch (leftmost <$> d)
        newInviteeId <- textInputOnInput "" (Object.singleton "class" "invitee-nameinput")
        newInviteeBtn <- buttonOnClick (pure (Object.singleton "class" "invitee-add")) $ text "Add"
        pure $ Tuple (AddInvitee <<< UserId <$> tagDyn newInviteeId newInviteeBtn) invitees

createRoomWidget :: forall m. MonadWidget m => SessionInfo -> m (Event RoomId)
createRoomWidget si =
  elClass "div" "create-room"
    $ do
        el "h2" $ text "Create a new room"

        alias <-
          elClass "div" "input-group" do
            el "h3" $ text "Canonical alias"
            alias <- textInputOnInput "" Object.empty
            text $ ":" <> si.homeserver
            pure alias
        invitees <-
          elClass "div" "input-group" do
            el "h3" $ text "Invitees"
            inviteesList
        affButtonLoopSimplified
          { ready:
            \er -> do
              case er of
                Just err -> text $ "Failed to create room: " <> message err
                Nothing -> pure unit
              createClicks <- buttonOnClick (pure Object.empty) $ text "Create room"
              pure
                $ tagDyn
                    ( do
                        a <- alias
                        i <- invitees
                        pure $ createRoom si a i
                    )
                    createClicks
          , loading:
            do
              pulseSpinner
              text "Creating room..."
          , success:
            \_ -> do
              text $ "Created successfully!"
          }
