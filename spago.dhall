{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "Purechat"
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
    , "uuid"
    , "web-storage"
    -- , "specular"
    , "strings"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
