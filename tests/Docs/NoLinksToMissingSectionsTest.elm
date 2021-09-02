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
        , test "should report a link to an unknown sibling section from declaration documentation" <|
            \() ->
                """module A.And.B exposing (a)
{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The link points to a definition that isn't exposed from any exposed module in this package."
                            , details =
                                [ "Links are only useful when they point to exposed package members."
                                , "Maybe you meant one of those: A.And.B, A.And.B.a."
                                ]
                            , under = "#b"
                            }
                        ]
        ]
