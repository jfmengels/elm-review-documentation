module Docs.ReviewAtDocsTest exposing (all)

import Docs.ReviewAtDocs exposing (rule)
import Elm.Project
import Json.Decode
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Docs.ReviewAtDocs"
        [ test "should not report an error when all @docs are correct" <|
            \() ->
                """module A exposing (D, T, a, b, c)

{-| Bla bla

@docs T, a, b
@docs c, D
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when an element has a @docs reference but function is not exposed" <|
            \() ->
                """module A exposing (a, b)

{-| Bla bla
@docs a, b, notExposed
-}
import B
a = 1
b = 2
notExposed = 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs reference for non-exposed `notExposed`"
                            , details = [ "REPLACEME" ]
                            , under = "notExposed"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 13 }, end = { row = 4, column = 23 } }
                        ]
        , test "should report an error when an element has a @docs reference but type is not exposed" <|
            \() ->
                """module A exposing (a, b)

{-| Bla bla
@docs a, b, NotExposed
-}
import B
a = 1
b = 2
type NotExposed = NotExposed
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs reference for non-exposed `NotExposed`"
                            , details = [ "REPLACEME" ]
                            , under = "NotExposed"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 13 }, end = { row = 4, column = 23 } }
                        ]
        , test "should not report an error when an element has a @docs reference and is exposed with exposing (..)" <|
            \() ->
                """module A exposing (..)

{-| Bla bla
@docs a, b, Exposed
-}
import B
a = 1
b = 2
type Exposed = Exposed
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when an unknown element has a @docs reference, with exposing (..)" <|
            \() ->
                """module A exposing (..)

{-| Bla bla
@docs a, b, Exposed
-}
import B
a = 1
b = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs reference for non-exposed `Exposed`"
                            , details = [ "REPLACEME" ]
                            , under = "Exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 13 }, end = { row = 4, column = 20 } }
                        ]
        , test "should report an error when encountering @docs on the first line of the module documentation (without space)" <|
            \() ->
                """module A exposing (a)

{-|@docs a
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs on the first line"
                            , details = [ "Using @docs on the first line will make for a broken documentation once published. Please move it to the beginning of the next line." ]
                            , under = "@docs"
                            }
                        ]
        , test "should report an error when encountering @docs on the first line of the module documentation (with space)" <|
            \() ->
                """module A exposing (a)

{-| @docs a
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs on the first line"
                            , details = [ "Using @docs on the first line will make for a broken documentation once published. Please move it to the beginning of the next line." ]
                            , under = "@docs"
                            }
                        ]
        , test "should report an error when encountering indented @docs" <|
            \() ->
                """module A exposing (a)

{-|
    @docs a
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found indented @docs"
                            , details = [ "@docs need to be at the beginning of a line, otherwise they can lead to broken documentation once published. on the first line will make for a broken documentation once published. Please remove the leading spaces" ]
                            , under = "@docs"
                            }
                        ]
        , test "should report an error when an element is exposed but has no @docs reference" <|
            \() ->
                """module A exposing (a, b, exposed)

{-| Bla bla
@docs a, b
-}
import B
a = 1
b = 2
exposed = 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing @docs reference for exposed `exposed`"
                            , details = [ "REPLACEME" ]
                            , under = "exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 26 }, end = { row = 1, column = 33 } }
                        ]
        , test "should not report errors when there is no @docs at all" <|
            \() ->
                """module A exposing (a)

{-| Bla bla
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report errors for exposed modules of a package even if there are no @docs at all" <|
            \() ->
                """module Exposed exposing (exposed)

{-| Bla bla
-}
import B
exposed = 1
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing @docs reference for exposed `exposed`"
                            , details = [ "REPLACEME" ]
                            , under = "exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 26 }, end = { row = 1, column = 33 } }
                        ]
        ]


package : Project
package =
    case Json.Decode.decodeString Elm.Project.decoder elmJson of
        Ok project ->
            Project.new
                |> Project.addElmJson
                    { path = "elm.json"
                    , raw = elmJson
                    , project = project
                    }

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
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""
