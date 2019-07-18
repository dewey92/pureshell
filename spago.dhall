{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "pure-shell"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "lists"
    , "aff"
    , "node-fs"
    , "node-fs-aff"
    , "monad-logger"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
