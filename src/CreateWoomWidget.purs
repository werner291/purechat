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
import Hareactive.Combinators (accum, shiftCurrent)
import Purechat.Types (RoomId, SessionInfo, UserId(..), unUserId)
import Turbine (component, list, modelView, use)
import Turbine.HTML (button, li, text)

inviteesList :: forall o. Component (Behavior (Set UserId)) o
inviteesList =
  component \{additions, removals} -> do
    invitees <-
      accum
        ( case _ of
            AddInvitee uid -> Set.insert uid
            RemoveInvitee uid -> Set.delete uid
        )
        Set.empty
        inviteeUpdate
    ul { class: pure "invitees" }
      $ list
          ( \uid ->
              li text (unUserId uid)
                </> button {} (text "X") `use` (\bo -> { removal: bo.click <#> const (RemoveInvitee uid) })
          )
          unUserId
      `use` (\removals -> shiftCurrent (map  removals))
          

-- removals <-
--   elClass "ul" "invitees" do
--     d <-
--       list (Set.toUnfoldable <$> invitees)
--         $ \duid ->
--             map switch $ dynamic $ duid
--               <#> ( \uid ->
--                     elClass "li" "invitee" do
--                       text $ unUserId uid
--                       r <- elemOnClick "i" (Object.singleton "class" "invitee-remove fas fa-times") (pure unit)
--                       pure $ const (RemoveInvitee uid) <$> r
--                 )
--     pure $ switch (leftmost <$> d)
-- newInviteeId <- textInputOnInput "" (Object.singleton "class" "invitee-nameinput")
-- newInviteeBtn <- buttonOnClick (pure (Object.singleton "class" "invitee-add")) $ text "Add"
-- pure $ Tuple (AddInvitee <<< UserId <$> tagDyn newInviteeId newInviteeBtn) invitees
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
              createClicks <- buttonOnClick (pure $ Object.singleton "class" "save") $ text "Create room"
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
