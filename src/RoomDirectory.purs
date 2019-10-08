
module RoomDirectory where

import Prelude

import Data.Array as A
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Hareactive.Combinators (accum, shiftCurrent, stepper)
import Hareactive.Interop (subscribe)
import Hareactive.Types (Behavior, Now, Stream)
import LoginComponent (loginPage)
import Turbine (Component, component, dynamic, list, output, runComponent, use, (</>))
import Turbine.HTML as E
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

import API (channelList)
import Purechat.Types (SessionInfo, RoomIndex, LoginToken(..), unToken)
import RemoteResource (RemoteResourceStatus(..))

-- | Component showing a list of all rooms the user is currently subscribed to.
-- | Outputs `selectChannel`, stream of channel IDs that the user has clicked on.
-- | In the future, may be made more smart with room searching and such.
roomListComponent :: RoomIndex -> Component {selectChannel :: Stream String} {}
roomListComponent ci =
  component \o -> do
    E.ul {} (
      (list (\rm -> (E.li {} $ E.text rm.name) `use` (\oo -> {selectChannel : const rm.name <$> oo.click})) (pure ci.channels) (\rm -> rm.name))
        `use` (\(oo::Behavior (Array {selectChannel :: Stream String})) -> {selectChannel: shiftCurrent (foldMap (\x -> x.selectChannel) <$> oo) })
    ) `output` {selectChannel: o.selectChannel}