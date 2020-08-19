module Documentation.NoMissingTest exposing (all)

import Documentation.NoMissing exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "REPLACEME"


details : List String
details =
    [ "REPLACEME" ]


all : Test
all =
    describe "Documentation.NoMissing"
        [ test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
function = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "function"
                            }
                        ]
        ]
