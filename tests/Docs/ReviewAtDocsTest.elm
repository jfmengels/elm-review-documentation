module Docs.ReviewAtDocsTest exposing (all)

import Docs.ReviewAtDocs exposing (rule)
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
        ]
