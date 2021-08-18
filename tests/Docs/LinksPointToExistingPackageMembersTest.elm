module Docs.LinksPointToExistingPackageMembersTest exposing (all)

import Docs.LinksPointToExistingPackageMembers exposing (rule)
import Elm.Project
import Json.Decode
import Review.Project as Project
import Review.Test
import Test exposing (Test, describe, test)


definitionInLinkNotExposedMessage : String
definitionInLinkNotExposedMessage =
    "The link points to a definition that isn't exposed from any exposed module in this package."


moduleInLinkNotExposed : String
moduleInLinkNotExposed =
    "The link points to a module that isn't listed in \"exposed-modules\"."


all : Test
all =
    describe "Docs.LinksPointToExistingPackageMembers"
        [ test "exposed function"
            (\() ->
                """module A.And.B exposing (a, b)

b =
    "b"

{-| Not [`b`](A-And-B#b).
-}
a =
    "a"
"""
                    |> Review.Test.runWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [ "A.And.B" ] })
                        )
                        rule
                    |> Review.Test.expectNoErrors
            )
        , test "exposed alias"
            (\() ->
                """module A.And.B exposing (a, B)

type alias B =
    B

{-| Not [`B`](A-And-B#B).
-}
a =
    "a"
"""
                    |> Review.Test.runWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [ "A.And.B" ] })
                        )
                        rule
                    |> Review.Test.expectNoErrors
            )
        , test "definition exposed in different module"
            (\() ->
                [ """module A exposing (a)

{-| Not [`b`](B#b).
-}
a =
    "a"
"""
                , """module B exposing (b)

b =
    "b"
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [ "A", "B" ] })
                        )
                        rule
                    |> Review.Test.expectNoErrors
            )
        , test "exposed all"
            (\() ->
                """module A.And.B exposing (..)

b =
    "b"

{-| Not [`b`](A-And-B#b).
-}
a =
    "a"
"""
                    |> Review.Test.runWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [ "A.And.B" ] })
                        )
                        rule
                    |> Review.Test.expectNoErrors
            )
        , describe "definition link"
            [ test "because it isn't exposed"
                (\() ->
                    """module A.And.B exposing (a)

b =
    "b"

{-| Not [`b`](A-And-B#b).
-}
a =
    "a"
"""
                        |> Review.Test.runWithProjectData
                            (Project.new
                                |> Project.addElmJson
                                    (elmJson { exposedModules = [ "A.And.B" ] })
                            )
                            rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = definitionInLinkNotExposedMessage
                                , details =
                                    [ "Links are only useful when they point to exposed package members."
                                    , "Maybe you meant one of those: A.And.B, A.And.B.a."
                                    ]
                                , under = "[`b`](A-And-B#b)"
                                }
                            ]
                )
            , test "because its module isn't in exposed-modules"
                (\() ->
                    """module A.And.B exposing (a, b)

b =
    "b"

{-| Not [`b`](A-And-B#b).
-}
a =
    "a"
"""
                        |> Review.Test.runWithProjectData
                            (Project.new
                                |> Project.addElmJson
                                    (elmJson { exposedModules = [] })
                            )
                            rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = definitionInLinkNotExposedMessage
                                , details =
                                    [ "Links are only useful when they point to exposed package members."
                                    , "Maybe you meant one of those: ."
                                    ]
                                , under = "[`b`](A-And-B#b)"
                                }
                            ]
                )
            ]
        , test "in readme because link doesn't point to member"
            (\() ->
                """module A exposing (a)

a =
    "a"
"""
                    |> Review.Test.runWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [ "A" ] })
                            |> Project.addReadme
                                { path = "README.md"
                                , content = "See [`B`](B)."
                                }
                        )
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = moduleInLinkNotExposed
                            , details =
                                [ "Links are only useful when they point to exposed package members."
                                , "Maybe you meant one of those: A, A.a."
                                ]
                            , under = "[`B`](B)"
                            }
                        ]
            )
        , test "in readme no module was specified for definition in link"
            (\() ->
                """module A exposing (a)

a =
    "a"
"""
                    |> Review.Test.runWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [ "A" ] })
                            |> Project.addReadme
                                { path = "README.md"
                                , content = "See [`a`](#a)."
                                }
                        )
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Using a link of the form [..](#definition) in the readme."
                            , details =
                                [ "There's no way to figure out in which module to look for this definition."

                                -- TODO REPLACEME This is improvable
                                , "I found `a` in the module(s) A.."
                                ]
                            , under = "[`a`](#a)"
                            }
                        ]
            )
        , test "module link because it isn't in exposed-modules"
            (\() ->
                """module A.And.B exposing (a, b)

b =
    "b"

{-| Not [`A.And.B`](A-And-B).
-}
a =
    "a"
"""
                    |> Review.Test.runWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [] })
                        )
                        rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = moduleInLinkNotExposed
                            , details =
                                [ "Links are only useful when they point to exposed package members."

                                -- TODO REPLACEME This is improvable
                                , "Maybe you meant one of those: ."
                                ]
                            , under = "[`A.And.B`](A-And-B)"
                            }
                        ]
            )
        , test "in file comment"
            (\() ->
                """module A.And.B exposing (a)

{-| Contains a and [`b`](A-And-B#b).
-}

b =
    "b"

a =
    "a"
"""
                    |> Review.Test.runWithProjectData
                        (Project.new
                            |> Project.addElmJson
                                (elmJson { exposedModules = [ "A.And.B" ] })
                        )
                        rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = definitionInLinkNotExposedMessage
                            , details =
                                [ "Links are only useful when they point to exposed package members."
                                , "Maybe you meant one of those: A.And.B, A.And.B.a."
                                ]
                            , under = "[`b`](A-And-B#b)"
                            }
                        ]
            )
        ]


createElmJson :
    String
    -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Json.Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = project
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


elmJson :
    { exposedModules : List String }
    -> { path : String, raw : String, project : Elm.Project.Project }
elmJson { exposedModules } =
    """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [

        """
        ++ (exposedModules
                |> List.map (\s -> "\"" ++ s ++ "\"")
                |> String.join ",\n        "
           )
        ++ """

    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
    }
}"""
        |> createElmJson
