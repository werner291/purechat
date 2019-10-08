module Purechat.Types where

import Prelude

type RoomIndex
  = { channels :: Array { name :: String } }

type SessionInfo
  = { token :: LoginToken, homeserver :: String }

newtype LoginToken
  = LoginToken String

unToken :: LoginToken -> String
unToken (LoginToken tok) = tok

instance tokenShow :: Show LoginToken where
  show (LoginToken lt) = "API auth token: " <> lt