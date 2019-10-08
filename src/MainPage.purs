module MainPage (mainPage) where

import Prelude
import API (channelList)
import Purechat.Types (RoomIndex, SessionInfo)
import RemoteResource (RemoteResourceStatus)
import Data.Array as A
import Data.Maybe (fromMaybe)
import Hareactive.Combinators (accum, shiftCurrent)
import Hareactive.Types (Behavior, Stream)
import ResourceSpinnerView (remoteResourceSpinnerView)
import RoomDirectory (roomListComponent)
import Turbine (Component, component, list, output, use, (</>))
import Turbine.HTML as E

newtype RoomId
  = RoomId String

showRoom :: SessionInfo -> RoomId -> Component {} {}
showRoom si (RoomId rid) =
  component \o ->
    E.text rid `output` {}

mainPage :: SessionInfo -> Component { logout :: Stream Unit } {}
mainPage si =
  component \(o :: { logout :: Stream Unit, openRoom :: Stream String }) -> do
    channels :: Behavior (RemoteResourceStatus RoomIndex) <- channelList si
    selectedChannels :: Behavior (Array (String)) <- accum A.cons [] o.openRoom
    ( E.div {}
        ( (remoteResourceSpinnerView channels roomListComponent)
            `use`
              (\oo -> { openRoom: shiftCurrent $ (fromMaybe (mempty :: Stream String) <<< map (_.selectChannel) <$> oo) })
        )
        </> E.button {} (E.text "Logout") `use` (\oo -> { logout: const unit <$> oo.click })
        </> list (showRoom si <<< RoomId) selectedChannels (\x -> x)
    )
      `output`
        { logout: o.logout }
