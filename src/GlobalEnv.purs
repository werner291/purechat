module Purechat.GlobalEnv where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Purechat.ServerFeed (KnownServerState)
import Purechat.Types (RemoteResourceView, RoomId, SessionInfo, UserId, UserProfile)
import Specular.FRP (Dynamic)
  
  -----------------
-- Environment --
-- Note: The GlovalEnv must store only things that are either globally relevant within the session
-- or cannot be dealt with deep within the view hierarchy.
-- It may be necessary to split this dictionary into more local environments.
-----------------

type GlobalEnv m = 
  { showProfile :: UserId -> Maybe UserProfile -> Effect Unit
  , session :: SessionInfo -- Deprecated, use dedicated functions where possible.
  , user_profile :: Dynamic (Maybe UserProfile)
  , channels_state :: Dynamic (RemoteResourceView (KnownServerState m))
  , logout :: Effect Unit
  , editProfile :: Effect Unit
  , updateProfile :: UserProfile -> Aff UserProfile
  , openRoom :: RoomId -> Effect Unit
  , createRoom :: Effect Unit
  , closeCurrentProfileCard :: Effect Unit
  }