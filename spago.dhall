{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "argonaut-core"
    , "behaviors"
    , "console"
    , "debug"
    , "effect"
    , "foldable-traversable"
    , "numbers"
    , "profunctor-lenses"
    , "psci-support"
    , "signal"
    , "turbine"
    , "uuid"
    , "web-storage"
    , "specular"
    , "strings"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
