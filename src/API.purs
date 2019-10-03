module API where

import Prelude
import Effect.Aff (Aff)
import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut ((~>), (:=))
import Data.Argonaut as JSON
import Data.Either (Either(..))

newtype LoginToken
  = LoginToken String

instance tokenShow :: Show LoginToken where
  show (LoginToken lt) = "API auth token: " <> lt

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
