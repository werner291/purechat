module LoginComponent where

import Prelude
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Hareactive.Combinators (filterJust, runStreamAff, snapshot, snapshotWith, stepper)
import Hareactive.Types (Stream)
import API (tryLogin, stringifyErrors)
import Purechat.Types (SessionInfo)
import Turbine (Component, component, output, use, (</>))
import Turbine.HTML as E

fromLeftMaybe :: forall a b. Either a b -> Maybe a
fromLeftMaybe (Left x) = Just x

fromLeftMaybe (Right _) = Nothing

loginPage :: Component { session :: Stream SessionInfo } {}
loginPage =
  component \on -> do
    let
      loginAttemptAsync = lift3 tryLogin on.username on.password on.homeserver
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
                    </> E.input { value: on.password, type: pure "password" } `use` (\o -> { password: o.value })
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