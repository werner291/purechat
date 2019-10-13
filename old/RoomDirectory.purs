
module RoomDirectory (roomListComponent) where

import Prelude

import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Hareactive.Combinators (shiftCurrent)
import Hareactive.Types (Behavior, Stream)
import Purechat.Types (RoomData)
import Turbine (class Key, Component, component, list, output, use)
import Turbine.HTML as E

listB :: forall a v b o k. Key k => (k -> Behavior v -> Component b o) 
                               -> Behavior (Array a) 
                               -> (a -> k)
                               -> (a -> v)
                               -> Component (Behavior (Array o)) {}
listB comp arrayB getKey = 
  let arrayOfBs = sequence arrayB
  in list comp arrayOfBs getKey

-- | Component showing a list of all rooms the user is currently subscribed to.
-- | Outputs `selectChannel`, stream of channel IDs that the user has clicked on.
-- | In the future, may be made more smart with room searching and such.
roomListComponent :: Behavior (Map String RoomData)
                  -> Component {selectChannel :: Stream String} {}
roomListComponent ci =
  let
    listEntry :: Behavior (Tuple String RoomData)
              -> Component {} {selectChannel::Stream (Tuple String (Behavior RoomData))}
    listEntry roomEntry = (E.li {} $ E.text (Tuple.fst roomEntry)) `use` (\oo -> {selectChannel : const roomEntry <$> oo.click})
  in
    component \o -> do
      E.ul {} (
        (list listEntry (map Map.toUnfoldable ci) (\(Tuple k v) -> k))
          `use` (\(oo::Behavior (Array {selectChannel :: Stream String})) -> {selectChannel: shiftCurrent (foldMap (\x -> x.selectChannel) <$> oo) })
      ) `output` {selectChannel: o.selectChannel}