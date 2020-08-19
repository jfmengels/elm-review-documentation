module Documentation.NoMissingTest exposing (all)

import Documentation.NoMissing exposing (everything, rule)
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
{-| module documentation -}
import Thing

function = 1
"""
                    |> Review.Test.run (rule everything)
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
{-| module documentation -}
import Thing

{-| documentation -}
function = 1
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function's documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing

{-| -}
function = 1
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "function"
                            }
                        ]
        , test "should report an error when a custom type does not have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
type CustomType = A
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = missingMessage
                            , details = missingDetails
                            , under = "CustomType"
                            }
                        ]
        , test "should not report an error when a custom type does have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| documentation -}
type CustomType = A
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectNoErrors
        , test "should report an error when a custom type's documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| -}
type CustomType = A
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "CustomType"
                            }
                        ]
        , test "should report an error when a type alias does not have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
type alias Alias = A
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = missingMessage
                            , details = missingDetails
                            , under = "Alias"
                            }
                        ]
        , test "should not report an error when a type alias does have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| documentation -}
type alias Alias = A
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectNoErrors
        , test "should report an error when a type alias' documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| -}
type alias Alias = A
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "Alias"
                            }
                        ]
        , test "should report an error when a module does not have documentation" <|
            \() ->
                """module A exposing (..)
import Thing
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = missingMessage
                            , details = missingDetails
                            , under = "A"
                            }
                        ]
        , test "should not report an error when a module does have documentation" <|
            \() ->
                """module A exposing (..)
{-| documentation -}
import Thing
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectNoErrors
        , test "should report an error when the module's documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| -}
import Thing
"""
                    |> Review.Test.run (rule everything)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "A"
                            }
                        ]
        ]
