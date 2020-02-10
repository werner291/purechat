module API.Profile where

import Prelude

import API.Core (getJsonAuthed, putJsonAuthed, responseOkOrBust, responseOkWithBody)
import Affjax (URL)
import Data.Argonaut (class EncodeJson, decodeJson, encodeJson, (.:?), (:=), (~>))
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff, error, throwError)
import Global (encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Purechat.Types (SessionInfo, UserId(..), UserProfile, unUserId)

getProfile :: SessionInfo -> UserId -> Aff UserProfile
getProfile si uid = case encodeURIComponent $ unUserId uid of
    Just encUid -> do
        json <- responseOkWithBody =<< (getJsonAuthed si $ "/_matrix/client/r0/profile/" <> encUid)
        let
            decoded = do
                o <- decodeJson json
                displayname :: Maybe String <- o .:? "displayname"
                avatar_url :: Maybe String <- o .:? "avatar_url"
                pure { displayname, avatar_url }
        case decoded of
            Right x -> pure x
            Left e -> throwError $ error e
    Nothing -> throwError $ error "UserId contains unencodeable characters."

-- unsafePartial should be safe since UserIDs shouldn't contain unencodeable characters
urlEncodeUserdId :: UserId -> String
urlEncodeUserdId (UserId uid) = (unsafePartial $ fromJust $ encodeURIComponent $ uid)

putProfileAttribute :: forall a. EncodeJson a => SessionInfo -> UserId -> String -> a -> Aff Unit
putProfileAttribute si uid key value = 
    let
    -- Body with message text and message type.
    reqBody :: JSON.Json
    reqBody =
        ( key := encodeJson value
            ~> JSON.jsonEmptyObject
        )
        
    path = "/_matrix/client/r0/profile/" <> (urlEncodeUserdId uid) <> "/" <> key
    in
    putJsonAuthed si path reqBody >>= responseOkOrBust

putProfileDisplayName :: SessionInfo -> Maybe String -> Aff Unit
putProfileDisplayName si displayname = putProfileAttribute si si.user_id "displayname" displayname

putProfileAvatarUrl :: SessionInfo -> Maybe URL -> Aff Unit
putProfileAvatarUrl si avatar_url = putProfileAttribute si si.user_id "avatar_url" avatar_url

putProfile :: SessionInfo -> UserProfile -> Aff UserProfile
putProfile si profile@{displayname,avatar_url} = do
    putProfileDisplayName si displayname
    putProfileAvatarUrl si avatar_url
    pure profile