module Purechat.GlobalEnv where

import Prelude

import API.Rooms (KnownServerState)
import Data.Maybe (Maybe)
import Effect (Effect)
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
  , openRoom :: RoomId -> Effect Unit
  , createRoom :: Effect Unit
  , closeCurrentProfileCard :: Effect Unit
  }