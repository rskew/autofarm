{-
-}
{ name = "event-scheduler"
, dependencies =
  [ "console"
  , "erl-pinto"
  ]
, packages = ./packages.dhall
, sources = [ "*.purs" ]
, backend = "purerl"
}
