module Docs.NoLinksToMissingSectionsTest exposing (all)

import Docs.NoLinksToMissingSections exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO From different modules
-- TODO From module documentation
-- TODO With ./
-- TODO With links like `[foo]: #b`
-- TODO With Forbid linking from exposed sections to non-exposed sections
-- TODO Report unknown modules
-- TODO Report links in the README
-- TODO Report links to non-existent things in README
-- TODO Report links to dependencies?
-- TODO Force links to be for the minimal version?
-- TODO Report when duplicate sections are found? Check if case sensitivity is important for ids.


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
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should not report a link to a known sibling section from declaration documentation when everything is exposed" <|
            \() ->
                """module A.And.B exposing (..)
b = 1

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from declaration documentation when everything is exposed" <|
            \() ->
                """module A.And.B exposing (..)
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
        , test "should consider an exposed type alias to be an existing section" <|
            \() ->
                """module A.And.B exposing (..)
type alias B = {}
{-| [link](#B)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
