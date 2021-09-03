module Docs.NoLinksToMissingSectionsTest exposing (all)

import Docs.NoLinksToMissingSections exposing (rule)
import Review.Project as Project
import Review.Test
import Test exposing (Test, describe, test)



-- TODO From module documentation
-- TODO With links like `[foo]: #b`
-- TODO With Forbid linking from exposed sections to non-exposed sections
-- TODO Report links in the README
-- TODO Report links to README when README does not exist
-- TODO Report links to non-existent things in README
-- TODO Report links to dependencies?
-- TODO Force links to be for the minimal version?
-- TODO Report when duplicate sections are found? Check if case sensitivity is important for ids.
-- TODO Report links to `#` or `Foo#` which are not useful?


all : Test
all =
    describe "Docs.NoLinksToMissingSections"
        [ test "should not report link to an existing sibling section from declaration documentation" <|
            \() ->
                """module A exposing (a, b)
b = 1

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from declaration documentation" <|
            \() ->
                """module A exposing (a)
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
                """module A exposing (..)
b = 1

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from declaration documentation when everything is exposed" <|
            \() ->
                """module A exposing (..)
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
        , test "should not report links to unknown external resources" <|
            \() ->
                """module A exposing (..)
{-|
[link](https://foo.com)
[link](./image.png)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an exposed type alias to be an existing section" <|
            \() ->
                """module A exposing (..)
type alias B = {}
{-| [link](#B)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an exposed custom type to be an existing section" <|
            \() ->
                """module A exposing (..)
type B = C
{-| [link](#B)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an exposed port to be an existing section" <|
            \() ->
                """module A exposing (..)
port b : Int -> Cmd msg

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an infix declaration to be an existing section (operator exists)" <|
            \() ->
                """module A exposing (..)
infix right 5 (++) = append

{-| [link](#++)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an infix declaration to be an existing section (operator doesn't exist)" <|
            \() ->
                """module A exposing (..)
{-| [link](#++)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#++"
                            }
                        ]
        , test "should consider a header inside a function documentation comment to be an existing section (h1)" <|
            \() ->
                """module A exposing (..)
{-|
# Section
-}
b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider a header inside a function documentation comment to be an existing section (h2)" <|
            \() ->
                """module A exposing (..)
{-|
## Section
-}
b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not consider a 'h7' header inside a function documentation comment to be an existing section" <|
            \() ->
                """module A exposing (..)
{-|
####### Section
-}
b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#section"
                            }
                        ]
        , Test.skip <|
            test "should slugify complex headings" <|
                \() ->
                    """module A exposing (..)
{-|
# Section with spaces and_ $things   [`links`](foo)
-}
b = 1
{-| [link](#section-with-spaces-and_-$things-links)
-}
a = 2
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
        , test "should report links to unknown modules" <|
            \() ->
                """module A exposing (..)
{-| [link](B-C)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module B.C"
                            , details = [ "This is a dead link." ]
                            , under = "B-C"
                            }
                        ]
        , test "should report links to sections in unknown modules" <|
            \() ->
                """module A exposing (..)
{-| [link](B#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module B"
                            , details = [ "This is a dead link." ]
                            , under = "B#b"
                            }
                        ]
        , test "should not report links to existing sections in a different module" <|
            \() ->
                [ """module A exposing (..)
{-| [link](B#b)
-}
a = 2
""", """module B exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections in a different module" <|
            \() ->
                [ """module A exposing (..)
{-| [link](B#b)
-}
a = 2
""", """module B exposing (c)
c = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Link points to a non-existing section or element"
                                , details = [ "This is a dead link." ]
                                , under = "B#b"
                                }
                            ]
                          )
                        ]
        , test "should not report links to existing sections in a different module (using ./)" <|
            \() ->
                [ """module A exposing (..)
{-| [link](./B#b)
-}
a = 2
""", """module B exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections in a different module (using ./)" <|
            \() ->
                [ """module A exposing (..)
{-| [link](./B#b)
-}
a = 2
""", """module B exposing (c)
c = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Link points to a non-existing section or element"
                                , details = [ "This is a dead link." ]
                                , under = "./B#b"
                                }
                            ]
                          )
                        ]
        , test "should not report links to existing sections inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](#a)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](#b)" } Project.new)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should not report links to existing sections from another module inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./A#a)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections from another module inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./A#b)" } Project.new)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "./A#b"
                            }
                        ]
        ]
