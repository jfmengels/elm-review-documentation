module Docs.NoLinksToMissingSectionsTest exposing (all)

import Docs.NoLinksToMissingSections exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO From different modules
-- TODO From module documentation
-- TODO With ./
-- TODO With links like `[foo]: #b`
-- TODO With Forbid linking from exposed to non-exposed elements


all : Test
all =
    describe "Docs.NoLinksToMissingSections"
        [ Test.skip <|
            test "should not report link to an existing sibling section from declaration documentation" <|
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
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        ]
