{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-ecs"
, dependencies =
  [ "canvas"
  , "console"
  , "effect"
  , "integers"
  , "ordered-collections"
  , "psci-support"
  , "record"
  , "strings"
  , "test-unit"
  , "transformers"
  , "typelevel-prelude"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
