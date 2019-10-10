
module RoomDirectory where

import Prelude

import Data.Argonaut (Json)
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Hareactive.Combinators (shiftCurrent)
import Hareactive.Types (Behavior, Stream)
import Purechat.Types (RoomData)
import Turbine (Component, component, list, output, use)
import Turbine.HTML as E

-- | Component showing a list of all rooms the user is currently subscribed to.
-- | Outputs `selectChannel`, stream of channel IDs that the user has clicked on.
-- | In the future, may be made more smart with room searching and such.
roomListComponent :: Map String RoomData -> Component {selectChannel :: Stream String} {}
roomListComponent ci =
  let
    listEntry roomName = (E.li {} $ E.text roomName) `use` (\oo -> {selectChannel : const roomName <$> oo.click})
  in
    component \o -> do
      E.ul {} (
        (list (\(Tuple k v) -> listEntry k) (pure $ Map.toUnfoldable ci) (\(Tuple k v) -> k))
          `use` (\(oo::Behavior (Array {selectChannel :: Stream String})) -> {selectChannel: shiftCurrent (foldMap (\x -> x.selectChannel) <$> oo) })
      ) `output` {selectChannel: o.selectChannel}