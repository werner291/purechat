module MainPage (mainPage) where

import Prelude
import API (longpollStream)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Hareactive.Combinators (accum, shiftCurrent)
import Hareactive.Types (Behavior, Stream)
import Purechat.Types (Event, MatrixRoomEvent(..), RoomData, SessionInfo)
import RoomDirectory (roomListComponent)
import Turbine (Component, component, dynamic, list, output, use, (</>))
import Turbine.HTML as E
import Purechat.RoomComponent (roomEventView, showRoom)

mainPage :: SessionInfo -> Component { logout :: Stream Unit } {}
mainPage si =
  component \(o :: { logout :: Stream Unit, openRoom :: Stream String }) -> do
    syncUpdates <- longpollStream si
    --account_events <- decodeJson <$> flattenArrayStream $ _.account_data <$> syncUpdates
    joined_rooms :: Behavior (Map String RoomData) <- accum Map.union Map.empty (_.rooms.join <$> syncUpdates)
    selectedChannels <- accum Array.cons [] o.openRoom
    let
      selectedChannelsWithData = do
        rooms_data <- joined_rooms
        selected_ids <- selectedChannels
        pure $ Array.mapMaybe (\k -> map (Tuple k) $ Map.lookup k rooms_data) selected_ids
    -- selectedChannels :: Behavior (Array (String)) <- accum A.cons [] o.openRoom
    ( E.div { id: pure "left-column" }
        ( ( let
              viewRooms :: Map String RoomData -> Component {} { selectChannel :: Stream String }
              viewRooms rm = roomListComponent rm `use` (\oo -> { selectChannel: oo.selectChannel })
            in
              dynamic (viewRooms <$> joined_rooms) `use` (\oo -> { openRoom: shiftCurrent (_.selectChannel <$> oo) })
          )
            </> (E.button {} (E.text "Logout") `use` (\oo -> { logout: const unit <$> oo.click }))
        )
        </> list (showRoom si) selectedChannelsWithData Tuple.fst
    )
      `output`
        { logout: o.logout }
