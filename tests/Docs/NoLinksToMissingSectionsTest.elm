module Docs.NoLinksToMissingSectionsTest exposing (all)

import Docs.NoLinksToMissingSections exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Docs.NoLinksToMissingSections"
        [ test "should not report link to an existing sibling section from declaration documentation" <|
            \() ->
                """module A.And.B exposing (a, b)
b = 1

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
