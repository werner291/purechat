module RemoteResource
  ( remoteResourceGet
  , RemoteResourceStatus(..)
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
import Purechat.Types

data RemoteResourceStatus a
  = ResourceLoading
  | ResourceLoaded a
  | ResourceError String

getJSON :: forall a. SessionInfo -> String -> (JSON.Json -> Either String a) -> Aff (Either String a)
getJSON session path decode = do
  resp <-
    AX.request
      ( AX.defaultRequest
          { url = (session.homeserver <> path)
          , responseFormat = AXRF.json
          , headers = [ RequestHeader "Authorization" ("Bearer " <> ((\(LoginToken tok) -> tok) session.token)) ]
          }
      )
  case resp.status of
    (StatusCode 200) -> case resp.body of
      Left e -> pure $ Left $ AX.printResponseFormatError e
      Right jsonBody ->
        let
          x :: Either String a
          x = decode jsonBody
        in
          pure x
    _ -> pure $ Left resp.statusText

instance rrsFunctor :: Functor RemoteResourceStatus where
  map f (ResourceLoaded a) = ResourceLoaded (f a)
  map _ ResourceLoading = ResourceLoading
  map _ (ResourceError e) = ResourceError e

stringifyErrors :: forall a. Either Error (Either String a) -> (Either String a)
stringifyErrors (Right x) = x
stringifyErrors (Left e) = Left (show e)

remoteResourceGet :: forall a. SessionInfo -> String -> (JSON.Json -> Either String a) -> Now (Behavior (RemoteResourceStatus a))
remoteResourceGet session path decode = do
  responseJSON <- runAffNow $ getJSON session path decode
  let
    eitherToStatus :: Either String a -> RemoteResourceStatus a
    eitherToStatus (Left e) = ResourceError e
    eitherToStatus (Right a) = ResourceLoaded a
  pure $ stepTo ResourceLoading (eitherToStatus <<< stringifyErrors <$> responseJSON)