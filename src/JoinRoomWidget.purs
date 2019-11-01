
module Purechat.JoinRoomWidget where

import Prelude
import API.Rooms (tryJoinRoom)
import CustomCombinators (affButtonLoopSimplified, pulseSpinner)
import Data.Maybe (Maybe(..))
import Effect.Aff (message)
import Purechat.Types (RoomId, SessionInfo, unRoomId)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)

joinRoomView :: forall m. MonadWidget m => SessionInfo -> RoomId -> m Unit
joinRoomView si rId = do
  el "p" $ text ("You are currently not participating in room " <> (unRoomId rId) <> " would you like to join it?")
  _ <-
    affButtonLoopSimplified
      { ready:
        \err -> do
          case err of
            Just e -> text $ "Failed to join room: " <> (message e)
            Nothing -> pure unit
          tryJoin <- buttonOnClick (pure mempty) (text "Join room")
          pure $ (const $ tryJoinRoom si (unRoomId rId)) <$> tryJoin
      , loading:
        do
          pulseSpinner
          text "Joining room..."
      , success: \_ -> text $ "Room successfully joined!"
      }
  pure unit