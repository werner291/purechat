
module API.Media where

import Prelude

import API.Core (pathToUri, authHeader, responseOkWithBody)
import Affjax (URL)
import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff, error, throwError)
import Purechat.Types (SessionInfo)
import Web.File.Blob (Blob)

mxcUrlToHttpUrl :: SessionInfo -> URL -> URL
mxcUrlToHttpUrl si url =
  case (String.stripPrefix (String.Pattern "mxc://") url) of
    Just mxc -> pathToUri si ("/_matrix/media/r0/download/" <> mxc)
    Nothing -> url

mxcUrlToThumbnailHttpUrl :: SessionInfo -> Int -> Int -> URL -> URL
mxcUrlToThumbnailHttpUrl si width height url =
  case (String.stripPrefix (String.Pattern "mxc://") url) of
    Just mxc -> pathToUri si ("/_matrix/media/r0/thumbnail/" <> mxc <> "?width=" <> (show width) <> "&height=" <> (show height))
    Nothing -> url

uploadMXC :: SessionInfo -> Blob -> Aff URL
uploadMXC si toUpload = do

  json <- (AX.request
    ( AX.defaultRequest
        { url = pathToUri si "/_matrix/media/r0/upload"
        , responseFormat = AXRF.json
        , headers = [ authHeader si ]
        , method = Left POST
        , content = Just $ RB.blob toUpload
        }
    ) >>= responseOkWithBody)

  case decodeJson json of
    Left err -> throwError $ error err
    Right ( dec :: { content_uri :: String} ) -> pure dec.content_uri