{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-ecs"
, dependencies =
  [ "console"
  , "effect"
  , "integers"
  , "ordered-collections"
  , "psci-support"
  , "record"
  , "strings"
  , "test-unit"
  , "transformers"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
