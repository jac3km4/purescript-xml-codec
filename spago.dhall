{ name = "xml-codec"
, dependencies =
    [ "prelude"
    , "assert"
    , "effect"
    , "maybe"
    , "either"
    , "functions"
    , "nullable"
    , "invariant-generic"
    , "invariant"
    , "tuples"
    , "integers"
    , "newtype"
    , "arrays"
    , "foldable-traversable"
    , "bifunctors"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
