module Documentation.NoMissingTest exposing (all)

import Documentation.NoMissing exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


missingMessage : String
missingMessage =
    "Missing documentation"


missingDetails : List String
missingDetails =
    [ "Documentation can help developers use this API." ]


all : Test
all =
    describe "Documentation.NoMissing"
        [ test "should report an error when a function does not have documentation" <|
            \() ->
                """module A exposing (..)
function = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = missingMessage
                            , details = missingDetails
                            , under = "function"
                            }
                        ]
        , test "should not report an error when a function does have documentation" <|
            \() ->
                """module A exposing (..)
{-| documentation -}
function = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function's documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| -}
function = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "function"
                            }
                        ]
        ]
