module Main where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (Error)
import Hareactive.Combinators (filterJust, nextOccurrence, runStreamAff, shiftCurrent, snapshot, snapshotWith, stepper)
import Hareactive.Types (Behavior, Stream, Future)
import Turbine (Component, component, dynamic, modelView, output, runComponent, use, (</>))
import Turbine.HTML as E



mainPage :: SessionInfo -> Component {} {}
mainPage si =
  component \o -> do -- style: "background-color: red;"
    ( E.div {}
        ( E.text "The login has logined." )
    ) `output` {}

app :: Component {} {}
app = component \o -> do

    session <- stepper Nothing (Just <$> (o.startSession :: Stream SessionInfo))

    let 
      pageToShow :: Behavior (Component {} { startSession :: Stream SessionInfo })
      pageToShow = do
        se <- session
        pure $ case se of
          Nothing -> loginPage `use` (\oo -> { startSession: oo.session })
          Just se -> mainPage se `use` (\oo -> { startSession: (mempty :: Stream SessionInfo) })
    
    (dynamic pageToShow `use` (\oo -> { startSession: shiftCurrent (_.startSession <$> oo) })) `output` {}

main :: Effect Unit
main = runComponent "#mount" loginPage
