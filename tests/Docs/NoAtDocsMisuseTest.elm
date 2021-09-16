module Docs.NoAtDocsMisuseTest exposing (all)

import Docs.NoAtDocsMisuse exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Docs.NoAtDocsMisuse"
        [ test "should not report an error when all @docs are correct" <|
            \() ->
                """module A exposing (D, T, a, b, d)

{-| Bla bla

@docs T, a, b
@docs d, D
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
                            , under = "unknown"
                            }
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
        ]
