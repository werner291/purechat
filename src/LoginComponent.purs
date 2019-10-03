module LoginComponent where

import Prelude
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (Error)
import Hareactive.Combinators (filterJust, nextOccurrence, runStreamAff, shiftCurrent, snapshot, snapshotWith, stepper)
import Hareactive.Types (Behavior, Stream, Future)
import API (LoginToken, tryLogin)
import Turbine (Component, component, dynamic, modelView, output, runComponent, use, (</>))
import Turbine.HTML as E

type SessionInfo
  = { token :: LoginToken, homeserver :: String }

fromLeftMaybe :: forall a b. Either a b -> Maybe a
fromLeftMaybe (Left x) = Just x

fromLeftMaybe (Right _) = Nothing

loginPage :: Component { session :: Stream SessionInfo } {}
loginPage =
  component \on -> do
    let
      loginAttemptAsync = do
        u <- on.username
        p <- on.password
        h <- on.homeserver
        pure $ tryLogin u p h

      stringifyErrors :: forall a. Either Error (Either String a) -> (Either String a)
      stringifyErrors (Right x) = x

      stringifyErrors (Left e) = Left (show e)
    loginAttemptResult <- runStreamAff $ snapshot loginAttemptAsync on.submit
    errorMsg <- stepper "" (filterJust $ (fromLeftMaybe <<< stringifyErrors) <$> loginAttemptResult)
    let
      tokens = filterJust $ (hush <<< stringifyErrors) <$> loginAttemptResult

      session = snapshotWith (\token homeserver -> { token, homeserver }) on.homeserver tokens
    ( E.div {}
        ( E.div {}
            ( E.label {} (E.text "Username")
                </> E.input { value: on.username } `use` (\o -> { username: o.value })
            )
            </> E.div {}
                ( E.label {} (E.text "Password")
                    </> E.input { value: on.password } `use` (\o -> { password: o.value })
                )
            </> E.div {}
                ( E.label {} (E.text "Homeserver")
                    </> E.input { value: on.homeserver } `use` (\o -> { homeserver: o.value })
                )
            </> E.button {} (E.text "Login") `use` (\o -> { submit: const unit <$> o.click })
            </> E.textB errorMsg
        )
    )
      `output`
        { session }
