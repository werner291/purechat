module Main where

import Prelude

import API (RemoteResourceStatus(..), SessionInfo, channelList, ChannelIndex, LoginToken(..), unToken)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Hareactive.Combinators (shiftCurrent, stepper)
import Hareactive.Interop (subscribe)
import Hareactive.Types (Behavior, Now, Stream)
import LoginComponent (loginPage)
import Turbine (Component, component, dynamic, list, output, runComponent, use, (</>))
import Turbine.HTML as E
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

roomListComponent :: ChannelIndex -> Component {} {}
roomListComponent ci =
  component \o -> do
    list (\rm -> E.text rm.name) (pure ci.channels) (\rm -> rm.name) `output` {}

    -- TODO figure out how to map output to perhaps a `Behavior (Maybe ...)` or something
remoteResourceSpinnerView :: forall a. Behavior (RemoteResourceStatus a) -> (a -> Component {} {})-> Component {} {}
remoteResourceSpinnerView res comp =
  let 
    viewToShow :: RemoteResourceStatus a -> Component {} {}
    viewToShow ResourceLoading = (E.text "Loading...") `use` (const {})
    viewToShow (ResourceError reason) = (E.text $ "Failed to load: " <> reason)  `use` (const {})
    viewToShow (ResourceLoaded a) = comp a
  in (const {} <$> (dynamic (viewToShow <$> res))) `use` (const {})

mainPage :: SessionInfo -> Component { logout :: Stream Unit } {}
mainPage si =
  component \o -> do

    channels :: Behavior (RemoteResourceStatus ChannelIndex) <- channelList si

    ( 
      E.div {} ( remoteResourceSpinnerView channels roomListComponent ) </>
      E.button {} ( E.text "Logout" ) `use` ( \oo -> {logout : const unit <$> oo.click} )
    ) `output` {logout:o.logout}

-- | A variant of the `stepper` combinator that is initialized with a Storage value.
-- | Whenever the signal produces a value, that value is stored to the Storage.
-- | A value of Nothing will delete the stored value.
storageStepper :: String -> Storage -> Stream (Maybe String) -> Now (Behavior (Maybe String))
storageStepper key storage updateSig = do
  
  initial <- liftEffect $ do
    let 
      storeX (Just x) = setItem key x storage
      storeX Nothing = removeItem key storage
    subscribe storeX updateSig
    getItem key storage
   
  stepper initial updateSig

app :: Component {} {}
app = component \o -> do

  storage <- liftEffect $ do
    w <- window
    localStorage w

  token <- storageStepper "token" storage ((_ >>= \ss -> Just (unToken ss.token)) <$> o.updateSession)
  homeserver <- storageStepper "homeserver" storage ((_ >>= \ss -> (Just ss.homeserver)) <$> o.updateSession)

  let 
    session = do
      t <- token
      h <- homeserver
      pure $ do 
        tt <- t
        hh <- h
        pure {
          token : (LoginToken tt),
          homeserver: hh
        }

  let 
    pageToShow :: Behavior (Component {} { updateSession :: Stream (Maybe SessionInfo) })
    pageToShow = do
      se <- session
      pure $ case se of
        Nothing -> loginPage `use` (\oo -> { updateSession: Just <$> oo.session })
        Just ses -> mainPage ses `use` (\oo -> { updateSession: const Nothing <$> oo.logout })
  
  (dynamic pageToShow `use` (\oo -> { updateSession: shiftCurrent (_.updateSession <$> oo) })) `output` {}


main :: Effect Unit
main = runComponent "#mount" app
