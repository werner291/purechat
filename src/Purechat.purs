module Purechat.Purechat (primaryView) where

import Prelude

import API.Profile (profileStore)
import CustomCombinators (dynamicMaybe_, elClass, pulseSpinner, remoteLoadingView)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Purechat.EditProfileWidget (editProfileWidget)
import Purechat.GlobalEnv (GlobalEnv)
import Purechat.ServerFeed (serverState)
import Purechat.Types (RoomId, SessionInfo, UserProfile, UserStatus)
import Purechat.Widgets.CreateRoomWidget (createRoomWidget)
import Purechat.Widgets.ProfileCard (profileCard)
import Purechat.Widgets.Sidebar (sidebar)
import RoomWidget (roomView)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, Event, dynamic_, never, newDynamic, readDynamic, switch)

data RoomViewState
  = PickRoom
  | ShowRoom RoomId
  | CreateRoom
  | EditProfile

data OutputCases
  = UserProfileOutput (Event UserProfile)
  | NoOutput

interruptedEvent :: forall a. Dynamic (Maybe (Event a)) -> Event a
interruptedEvent d =
  switch $ d
    >>= case _ of
        Just a -> pure a
        Nothing -> pure never

mainView :: forall m. MonadWidget m => GlobalEnv m -> Dynamic RoomViewState ->  m Unit
mainView env currentRoomView = do
  elClass "div" "main-view" $ dynamic_ $ currentRoomView
    <#> case _ of
        EditProfile -> do
          dynamic_ $ env.user_profile <#> case _ of
            Nothing -> do
              pulseSpinner
              text "Loading profile..."
            Just prof -> editProfileWidget env prof
        CreateRoom -> do
          createRoomWidget env
        ShowRoom rid -> do
          _ <- remoteLoadingView env.channels_state pulseSpinner
            $ \st -> do
                -- Note: We specifically take a snapshot of the joined_rooms here!
                -- We cannot simply map as that would cause the dynamic to update when unrelated rooms change.
                -- Perhaps this shows that `Dynamic (Map k v)` isn't quite the right idea here.
                -- Maybe some structure where you can subscribe to updates/insertions/deletions,
                -- similar to how fanOutM works?
                roomD <- readDynamic (Map.lookup rid <$> st.joined_rooms)
                -- Yuck, the `pure` here should be a Dynamic that's dedicated to that room.
                roomView env rid (pure roomD)
          pure unit
        PickRoom -> do
          elClass "div" "no-room-selected" do
            text "Welcome! Please select a room to get started."

userStateToProfile :: UserStatus -> UserProfile
userStateToProfile ust = {avatar_url:ust.avatar_url, displayname:ust.displayname}

-- class Interruptible a b | a -> b where
--   interrupted :: a -> b

-- instance eventInterruptible :: Interruptible (Event a) (Event a) where
--   interrupted ev = ev

-- dynamicMergeOutputs :: forall a m. Interruptible a. MonadWidget m => 
  
-- componentA :: forall m. MonadWidget m => m { a :: Event Int }
-- componentA = ?impl_a

-- componentB :: forall m. MonadWidget m => m { b :: Event String }
-- componentB = ?impl_b


-- dynMonoid :: forall sA sB sU m. Record.Builder sA sB sU => Monoid a => MonadWidget m => Dynamic (m a) -> m (Dynamic a)
-- dynMonoid = ?wut

-- switchEvents :: forall a b. Dynamic (Record (a :: Event a, b ::Event b))-> { a :: Event a, b ::Event b }
-- switchEvents inpts = ?wut

-- The "primary" widget that is visible once the user is logged in .
primaryView :: forall m. MonadWidget m => MonadFRP m => SessionInfo -> { logout :: Effect Unit } -> m Unit
primaryView si env = do
  currentRoomView <- newDynamic PickRoom
  currentProfileCard <- newDynamic Nothing
  server_state <- serverState si

  user_profile <- profileStore si

  -- user_profile :: Dynamic (Maybe UserProfile) <- holdFirst $ filterJustEvent $ changed $ do
  --   map fromRRLoaded server_state >>=
  --     case _ of
  --       Just st -> do
  --         pres <- st.global_presence
  --         case Map.lookup si.user_id pres of
  --           Just d -> (Just <<< userStateToProfile) <$> d
  --           Nothing -> pure Nothing
  --       Nothing -> pure Nothing

  let
    inner_env :: GlobalEnv m
    inner_env =
        { createRoom: currentRoomView.set CreateRoom
        , editProfile: currentRoomView.set EditProfile
        , openRoom: \rId -> currentRoomView.set (ShowRoom rId)
        , channels_state: server_state
        , user_profile: user_profile.currentProfile
        , updateProfile : user_profile.updateProfile
        , logout: env.logout
        , session: si
        , showProfile: \userId profile -> currentProfileCard.set (Just { userId, profile })
        , closeCurrentProfileCard: currentProfileCard.set Nothing
        }
  dynamicMaybe_ currentProfileCard.dynamic (profileCard inner_env)
  elClass "div" "main-container" $ sidebar inner_env