module Docs.ReviewLinksAndSectionsTest exposing (all)

import Docs.ReviewLinksAndSections exposing (rule)
import Elm.Project
import Json.Decode
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO Report links to dependencies?
-- TODO Force links to dependencies to be for the minimal version?
-- TODO Report links to `#` or `Foo#` which are not useful?
-- TODO Report unused `[foo]: #b` links?
-- TODO Report images that are relative (in packages and in exposed sections). They should be linking to images hosted on GitHub for instance.
-- TODO Enforce that the readme uses links that link to GitHub. Only for packages.


all : Test
all =
    describe "Docs.ReviewLinksAndSections"
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
        , test "should not report link to an existing sibling section from module documentation" <|
            \() ->
                """module A exposing (a, b)
{-| [link](#b)
-}

import B
b = 1
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from module documentation" <|
            \() ->
                """module A exposing (a)
{-| [link](#b)
-}

import B
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
        , test "should not report link to an existing sibling section from port documentation" <|
            \() ->
                """module A exposing (a, b)
b = 1
{-| [link](#b)
-}
port a : String -> Cmd msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from port documentation" <|
            \() ->
                """module A exposing (a)
{-| [link](#b)
-}
port a : String -> Cmd msg
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
        , test "should not report link shortcuts that link to existing sections" <|
            \() ->
                """module A exposing (a, b)
b = 1
{-| this is a [link] shortcut?

[link]: #b
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report link shortcuts that link to missing sections" <|
            \() ->
                """module A exposing ()
{-| this is a [link] shortcut?

[link]: #b

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
        , test "should consider a header inside a module documentation comment to be an existing section" <|
            \() ->
                """module A exposing (..)
{-|
# Section
-}

import B

b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should slugify complex headings" <|
            \() ->
                """module A exposing (..)
{-|
# Section *with* ~some~ _spaces_ and\\_ $thi.ngs . [`links`](foo)

### `section`
### question?

-}
b = 1
{-|
[1](#section-_with_-some-_spaces_-and-_-thi-ngs-links-foo-)
[2](#-section-)
[3](#question-)
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
        , test "should not report links to existing sections inside the README (using ./)" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](./#a)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections inside the README (using ./)" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](./#b)" } Project.new)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "./#b"
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
        , test "should report links to README when there is no README (without slug)" <|
            \() ->
                """module A exposing (a)              
{-| [link](./)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to missing README"
                            , details = [ "elm-review only looks for a 'README.md' located next to your 'elm.json'. Maybe it's positioned elsewhere or named differently?" ]
                            , under = "./"
                            }
                        ]
        , test "should report links to README when there is no README (with slug)" <|
            \() ->
                """module A exposing (a)
{-| [link](./#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to missing README"
                            , details = [ "elm-review only looks for a 'README.md' located next to your 'elm.json'. Maybe it's positioned elsewhere or named differently?" ]
                            , under = "./#b"
                            }
                        ]
        , test "should not report links from non-exposed modules to non-exposed modules" <|
            \() ->
                [ """module NotExposed exposing (a)
{-| [link](./AlsoNotExposed)
-}
a = 2
""", """module AlsoNotExposed exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report links from non-exposed modules to exposed modules" <|
            \() ->
                """module NotExposed exposing (a)
{-| [link](./Exposed)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report links from exposed modules to exposed modules" <|
            \() ->
                """module Exposed2 exposing (a)
{-| [link](./Exposed)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report links from exposed modules to non-exposed modules" <|
            \() ->
                [ """module Exposed2 exposing (a)
{-| [link](./NotExposed)
-}
a = 2
""", """module NotExposed exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Exposed2"
                          , [ Review.Test.error
                                { message = "Link in public documentation points to non-exposed module"
                                , details = [ "Users will not be able to follow the link." ]
                                , under = "./NotExposed"
                                }
                            ]
                          )
                        ]
        , test "should not report links from non-exposed sections to non-exposed sections" <|
            \() ->
                [ """module Exposed2 exposing (b)
b = a

{-| [link](./Exposed3#hidden)
-}
a = 1
""", """module Exposed3 exposing (exposed)
exposed = 2

{-|
# Hidden
-}
a = 3
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report links from non-exposed sections to exposed sections" <|
            \() ->
                [ """module Exposed2 exposing (b)
b = a

{-| [link](./Exposed3#exposed)
-}
a = 1
""", """module Exposed3 exposing (exposed)
exposed = 2

{-|
# Hidden
-}
a = 3
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report links from exposed sections to non-exposed sections" <|
            \() ->
                [ """module Exposed2 exposing (b)
{-| [link](./Exposed3#hidden)
-}
b = 1
""", """module Exposed3 exposing (exposed)
exposed = 2

{-|
# Hidden
-}
a = 3
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Exposed2"
                          , [ Review.Test.error
                                { message = "Link in public documentation points to non-exposed section"
                                , details = [ "Users will not be able to follow the link." ]
                                , under = "./Exposed3#hidden"
                                }
                            ]
                          )
                        ]
        , test "should report links from README to non-exposed modules" <|
            \() ->
                """module NotExposed exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./NotExposed)" } packageProject)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link in public documentation points to non-exposed module"
                            , details = [ "Users will not be able to follow the link." ]
                            , under = "./NotExposed"
                            }
                        ]
        , test "should not report links from README to exposed modules" <|
            \() ->
                """module NotExposed exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./Exposed)" } packageProject)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links from README to non-exposed sections" <|
            \() ->
                """module Exposed2 exposing (a)
a = 1
{-|
# Section
-}
b = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./Exposed2#section)" } packageProject)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link in public documentation points to non-exposed section"
                            , details = [ "Users will not be able to follow the link." ]
                            , under = "./Exposed2#section"
                            }
                        ]
        , test "should not report links from README to non-exposed sections in non-package projects" <|
            \() ->
                """module Exposed2 exposing (a)
a = 1
{-|
# Section
-}
b = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./Exposed2#section)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report duplicate sections (declaration doc comment)" <|
            \() ->
                """module Exposed2 exposing (error, exposed)
{-|
# Error
-}
exposed = 1

error = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Error"
                            }
                        ]
        , test "should report duplicate sections (module doc comment)" <|
            \() ->
                """module Exposed2 exposing (error)
{-|
# Error
-}

error = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Error"
                            }
                        ]
        , test "should report duplicate sections (all in declaration doc comments)" <|
            \() ->
                """module Exposed2 exposing (error)
{-|
# Some section

# Some Section
-}

error = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Some Section"
                            }
                        ]
        , test "should report duplicate sections in README" <|
            \() ->
                """module Exposed2 exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = """
# Some section

# Some Section
""" } packageProject)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Some Section"
                            }
                        ]
        ]


packageProject : Project
packageProject =
    case Json.Decode.decodeString Elm.Project.decoder elmJson of
        Ok project ->
            Project.new
                |> Project.addElmJson
                    { path = "elm.json"
                    , raw = elmJson
                    , project = project
                    }
                |> Project.addModule { path = "src/Exposed", source = "module Exposed exposing (exposed)\nexposed = 1" }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


elmJson : String
elmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed",
        "Exposed2",
        "Exposed3"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""
