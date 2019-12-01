module LoginComponent where

import Prelude

import API.Core (tryLogin)
import Control.Apply (lift3)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (message)
import Hareactive.Combinators (filterJust, runStreamAff, snapshot, stepper)
import Hareactive.Types (Behavior, Stream)
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
      loginAttemptAsync :: Behavior (Aff SessionInfo)
      loginAttemptAsync = lift3 tryLogin on.username on.password on.homeserver
    loginAttemptResult :: Stream (Either Aff.Error SessionInfo) <- runStreamAff $ snapshot loginAttemptAsync on.submit
    errorMsg <- stepper "" (map message <$> filterJust $ fromLeftMaybe <$> loginAttemptResult)
    let
      session = filterJust $ hush <$> loginAttemptResult
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