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
    , "effect"
    , "foldable-traversable"
    , "halogen"
    , "halogen-css"
    , "halogen-vdom"
    , "numbers"
    , "profunctor-lenses"
    , "psci-support"
    , "signal"
    , "turbine"
    , "web-storage"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
