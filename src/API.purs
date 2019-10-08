module API
  ( tryLogin
  , stringifyErrors
  , channelList
  ) where

import Prelude
import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut ((:=), (~>))
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Effect.Aff (Aff, Error)
import Hareactive.Combinators (runAffNow, stepTo)
import Hareactive.Types (Behavior, Now)
import RemoteResource
import Purechat.Types

stringifyErrors :: forall a. Either Error (Either String a) -> (Either String a)
stringifyErrors (Right x) = x

stringifyErrors (Left e) = Left (show e)

-- | Attempt to obtain a login token using the provided username 
-- | and password pair on the given homeserver uri.
-- | Aff returns either a string error or a Login token.
-- | TODO: This thing is rather dumb and does not take .well-known and such into account.
tryLogin :: String -> String -> String -> Aff (Either String LoginToken)
tryLogin username password homeserver = do
  let
    reqBody :: JSON.Json
    reqBody =
      ( "type" := "m.login.password"
          ~> "identifier"
          := ("type" := "m.id.user" ~> "user" := username ~> JSON.jsonEmptyObject)
          ~> "password"
          := password
          ~> JSON.jsonEmptyObject
      )
  response <- AX.post AXRF.json (homeserver <> "/_matrix/client/r0/login") (RB.json reqBody)
  pure
    $ case response.status of
        (StatusCode 200) -> case response.body of
          Right json -> case JSON.decodeJson json >>= (\o -> JSON.getField o "access_token") >>= JSON.decodeJson of
            Left err -> Left err
            Right tok -> Right $ LoginToken tok
          _ -> Left "Server returned invalid JSON."
        (StatusCode 401) -> Left "Authentication failed, please verify credentials."
        _ -> Left $ "Unexpected server response HTTP " <> show (response.statusText)

channelList :: SessionInfo -> Now (Behavior (RemoteResourceStatus RoomIndex))
channelList si =
  remoteResourceGet si "/_matrix/client/r0/joined_rooms"
    $ \json -> do
        o <- JSON.decodeJson json
        rooms :: Array String <- JSON.getField o "joined_rooms" >>= JSON.decodeJson
        pure
          { channels: { name: _ } <$> rooms
          }
