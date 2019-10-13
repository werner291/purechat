module MainPage (mainPage) where

import Prelude

import API (longpollStream)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Debug.Trace (spy)
import Hareactive.Combinators (accum, shiftCurrent)
import Hareactive.Types (Behavior, Stream)
import Purechat.RoomComponent (showRoom)
import Purechat.Types (RoomData, SessionInfo)
import RoomDirectory (roomListComponent)
import Turbine (Component, component, dynamic, list, output, use, (</>))
import Turbine.HTML as E

-- | RoomData objects, in a sense, represent "everything the client knows about a given room at some point in time."
-- | Sometimes, the server will send us new data about the room, which can be merged with other data to get a more
-- | complete picture. In the future, it would be preferable to split up these things into behaviors and streams.
mergeRoomData :: RoomData -> RoomData -> RoomData
mergeRoomData {timeline:{events:evA}} {timeline:{events:evB}} = {timeline:{events:evA <> evB}}

-- | A component containing what could be considered the "main" view of the application after logging in.
-- | It takes a `SessionInfo` record as input 
mainPage :: SessionInfo -> Component { logout :: Stream Unit } {}
mainPage si =
  component \(o :: { logout :: Stream Unit, openRoom :: Stream String }) -> do
    syncUpdates <- longpollStream si
    joined_rooms :: Behavior (Map String RoomData) <- accum (Map.unionWith mergeRoomData) Map.empty (_.rooms.join <$> syncUpdates)

    -- let
    --   selectChannel :: String -> Behavior (Maybe RoomData)
    --   selectChannel k = Map.lookup k <$> joined_rooms
      -- selectedChannelsWithData :: Behavior (Array (Tuple String (Behavior RoomData)))
      -- selectedChannelsWithData = do
      --   room_data <- joined_rooms
      --   selected <- selectedChannels
      --   pure $ Array.mapMaybe (\k -> (Tuple k <<< pure) <$> Map.lookup k room_data) selected
    
    selectedChannels <- accum Array.cons [] o.openRoom
        
    ( E.div { id: pure "left-column" }
        ( ( 
            let
              viewRooms :: Behavior (Map String RoomData)
                        -> Component {} { selectChannel :: Stream (Tuple String RoomData) }
              viewRooms rm = roomListComponent rm `use` (\oo -> { selectChannel: oo.selectChannel })
            in
              dynamic (viewRooms joined_rooms) `use` (\oo -> { openRoom: shiftCurrent (_.selectChannel <$> oo) })
          )
            </> (E.button {} (E.text "Logout") `use` (\oo -> { logout: const unit <$> oo.click }))
        )
        </> 
          list (\(Tuple rid rdata) -> showRoom si rid rdata) selectedChannels identity
    )
      `output`
        { logout: o.logout }
