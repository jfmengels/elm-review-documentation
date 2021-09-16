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
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when an element has a @docs reference but element is not exposed" <|
            \() ->
                """module A exposing (a, b)

{-| Bla bla
@docs a, b, unknown
-}
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs reference for non-exposed `unknown`"
                            , details = [ "REPLACEME" ]
                            , under = "unknown"
                            }
                        ]
        ]
