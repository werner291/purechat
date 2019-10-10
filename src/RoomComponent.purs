
module Purechat.RoomComponent (roomEventView, showRoom) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Purechat.Types (Event, MatrixRoomEvent(..), SessionInfo, RoomData)
import Turbine (Component, component, output, (</>), list)
import Turbine.HTML as E

-- | Component displaying a single room event.
roomEventView :: Event MatrixRoomEvent -> Component {} {}
roomEventView e =
  component \o ->
    ( case e.content of
        Left evtString ->
          E.div {} ( E.p {} (E.text $ "Unknown event: " <> evtString <> " Sent by: " <> e.sender) )
        Right evtContent -> case evtContent of
          Message b ->
            E.div {class: pure "message-wrapper"}
              ( E.div {class: pure "username"} (E.text e.sender)
                  </> E.div {} (E.text b.body)
              )
    )
      `output`
        {}

-- | Component displaying a room that the user is participating in.
showRoom :: SessionInfo -> (Tuple String RoomData) -> Component {} {}
showRoom si (Tuple rid rdata) =
    component \o ->
    ( E.div { class: pure "room-view" }
        ( (E.div { class: pure "room-title" } (E.text rid))
            </> E.div { class: pure "room-messages" } (list roomEventView (pure rdata.timeline.events) (_.event_id))
            </> E.div { class: pure "message-form" } (E.textarea {} </> E.button {} (E.text "Send"))
        )
    )
        `output`
        {}