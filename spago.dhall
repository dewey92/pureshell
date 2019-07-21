{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "pure-shell"
, dependencies =
    [ "aff"
    , "console"
    , "debug"
    , "effect"
    , "lists"
    , "monad-logger"
    , "node-fs"
    , "node-fs-aff"
    , "optparse"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
