
module Purechat.RoomComponent (roomEventView, showRoom) where

import Prelude

import API as API
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Maybe
import Hareactive.Combinators (changes, filter, runStreamAff, snapshot, stepper)
import Hareactive.Types (Behavior)
import Purechat.Types (Event, MatrixRoomEvent(..), SessionInfo, RoomData)
import Turbine (Component, component, list, output, use, (</>))
import Turbine.HTML as E

-- | Component displaying a single room event.
roomEventView :: SessionInfo -> Event MatrixRoomEvent -> Component {} {}
roomEventView si e =
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
showRoom :: SessionInfo -> String -> (Behavior RoomData) -> Component {} {}
showRoom si rid rdata =
    component \o -> do

        sendResult <- runStreamAff $ API.sendMessage si rid <$> snapshot o.messageComposing o.send

        msg <- stepper "" $ (changes o.messageComposing) <> (const "" <$> (filter (Maybe.isLeft) sendResult))

        ( E.div { class: pure "room-view" }
            ( (E.div { class: pure "room-title" } (E.text rid))
                </> E.div { class: pure "room-messages" } (
                  list (roomEventView si) (_.timeline.events <$> rdata) (_.event_id)
                  </>
                  E.textB ((show <<< Array.length <<< _.timeline.events) <$> rdata)
                )
                </> E.div { class: pure "message-form" } (
                        (E.textarea { value: msg } `use` (\oo -> { messageComposing: oo.value }))
                            </> (E.button {} (E.text "Send") `use` (\oo -> { send: const unit <$> oo.click }))
                    )
            )
        ) `output` {}