{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "filterable"
    , "integers"
    , "node-buffer"
    , "node-fs"
    , "psci-support"
    , "strings"
    , "stringutils"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
