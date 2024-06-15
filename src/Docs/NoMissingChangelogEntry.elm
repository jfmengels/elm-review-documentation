module Docs.NoMissingChangelogEntry exposing
    ( rule
    , Configuration, defaults, withPathToChangelog
    )

{-|

@docs rule

    config =
        [ Docs.NoMissingChangelogEntry.rule Docs.NoMissingChangelogEntry.defaults
        ]

@docs Configuration, defaults, withPathToChangelog


## Example

Given the following `CHANGELOG.md` file:

    # Changelog

    ## [Unreleased]

    Stuff happened

    ## 1.2.0

    More stuff happened

    ## 1.1.0

    Stuff happened

    [Unreleased]: https://github.com/author/package-name/compare/v1.2.0...HEAD
    [1.2.0]: https://github.com/author/package-name/releases/tag/1.2.0
    [1.1.0]: https://github.com/author/package-name/releases/tag/1.1.0

If the current version is `1.2.0`, then there won't be any error.
If the current version is `1.2.1`, then an error will be reported,
and a fix will be suggested to fix to the following:

    # Changelog

    ## [Unreleased]

    ## [1.2.1]

    Stuff happened

    ## 1.2.0

    More stuff happened

    ## 1.1.0

    Stuff happened

    [Unreleased]: https://github.com/author/package-name/compare/v1.2.1...HEAD
    [1.2.1]: https://github.com/author/package-name/releases/tag/1.2.1
    [1.2.0]: https://github.com/author/package-name/releases/tag/1.2.0
    [1.1.0]: https://github.com/author/package-name/releases/tag/1.1.0


## When (not) to enable this rule

This rule is useful only when the project is an Elm package
and you would like to have an automated .


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review/example --rules Docs.NoMissingChangelogEntry
```

-}

import Dict exposing (Dict)
import Elm.Package
import Elm.Project exposing (Project)
import Elm.Version
import Review.FilePattern as FilePattern
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports when `CHANGELOG.md` is missing an entry for the current version of the Elm package.
-}
rule : Configuration -> Rule
rule (Configuration { changelogPath }) =
    Rule.newProjectRuleSchema "Docs.NoMissingChangelogEntry" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withExtraFilesProjectVisitor (extraFilesVisitor changelogPath) [ FilePattern.include (getChangelogPath changelogPath) ]
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


{-| Configuration for the rule.
-}
type Configuration
    = Configuration { changelogPath : Maybe String }


{-| Default configuration for the rule.
Considers the changelog to be `CHANGELOG.md` next to the project's `elm.json`.
-}
defaults : Configuration
defaults =
    Configuration { changelogPath = Nothing }


{-| Define the path to the changelog.

    config =
        [ Docs.NoMissingChangelogEntry.defaults
            |> Docs.NoMissingChangelogEntry.withPathToChangelog "path/to/changelog.txt"
            |> Docs.NoMissingChangelogEntry.rule
        ]

-}
withPathToChangelog : String -> Configuration -> Configuration
withPathToChangelog changelogPath _ =
    Configuration { changelogPath = Just changelogPath }


getChangelogPath : Maybe String -> String
getChangelogPath changelogPath =
    Maybe.withDefault "CHANGELOG.md" changelogPath


type alias ProjectContext =
    { elmJsonData : Maybe ElmJsonData
    }


type alias ElmJsonData =
    { packageName : String
    , version : String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { elmJsonData = Nothing
    }


elmJsonVisitor : Maybe { a | project : Project } -> ProjectContext -> ( List (Rule.Error scope), ProjectContext )
elmJsonVisitor maybeElmJsonData context =
    case maybeElmJsonData of
        Just { project } ->
            case project of
                Elm.Project.Package { name, version } ->
                    ( []
                    , { context
                        | elmJsonData =
                            Just
                                { packageName = Elm.Package.toString name
                                , version = Elm.Version.toString version
                                }
                      }
                    )

                Elm.Project.Application _ ->
                    ( [ Rule.globalError
                            { message = "The Elm project is unexpectedly an application"
                            , details = [ "This rule only supports Elm packages, but doesn't support Elm applications as they don't have a version number. I recommend that you remove this rule from your review configuration." ]
                            }
                      ]
                    , context
                    )

        Nothing ->
            ( [], context )


extraFilesVisitor : Maybe String -> Dict String { fileKey : Rule.ExtraFileKey, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
extraFilesVisitor maybeChangelogPath files context =
    let
        changelogPath : String
        changelogPath =
            getChangelogPath maybeChangelogPath
    in
    case Dict.get changelogPath files of
        Just { fileKey, content } ->
            case context.elmJsonData of
                Nothing ->
                    ( [], context )

                Just elmJsonData ->
                    if elmJsonData.version == "1.0.0" || String.contains elmJsonData.version content then
                        ( [], context )

                    else
                        ( [ reportError fileKey elmJsonData content ]
                        , context
                        )

        Nothing ->
            ( reportErrorWhenChangelogIsNotFound
                { changelogPath = maybeChangelogPath
                , elmJsonVersion = Maybe.map .version context.elmJsonData
                }
            , context
            )


reportErrorWhenChangelogIsNotFound : { changelogPath : Maybe String, elmJsonVersion : Maybe String } -> List (Rule.Error scope)
reportErrorWhenChangelogIsNotFound { changelogPath, elmJsonVersion } =
    case elmJsonVersion of
        Nothing ->
            []

        Just "1.0.0" ->
            -- TODO Report an error with a changelog skeleton
            []

        Just _ ->
            case changelogPath of
                Nothing ->
                    [ Rule.globalError
                        { message = "Could not find the CHANGELOG.md file"
                        , details =
                            [ "I was looking for the CHANGELOG.md file next to your project's elm.json file but couldn't find it. Please make sure that the spelling is correct."
                            , "If your changelog is named differently or is in a different location, then you can configure this rule to look for it in a different location:"
                            , """    config =
        [ Docs.NoMissingChangelogEntry.defaults
            |> Docs.NoMissingChangelogEntry.withPathToChangelog "path/to/your/changelog.md"
            |> Docs.NoMissingChangelogEntry.rule
        ]"""
                            , "Note that the path is relative your project's elm.json file."
                            ]
                        }
                    ]

                Just customPath ->
                    [ Rule.globalError
                        { message = "Could not find the " ++ customPath ++ " changelog file"
                        , details =
                            [ "I was looking for the " ++ customPath ++ " changelog file but couldn't find it. Please make sure that the path you specified through Docs.NoMissingChangelogEntry.withPathToChangelog is correct."
                            , "Also note that the path you specify has to be relative to your project's elm.json file."
                            ]
                        }
                    ]


reportError : Rule.ExtraFileKey -> ElmJsonData -> String -> Rule.Error scope
reportError fileKey elmJsonData content =
    let
        lines : List String
        lines =
            String.lines content

        unreleased : Maybe ( Int, String )
        unreleased =
            findLineWithUnreleased 0 lines

        unreleasedLinkLine : Maybe Int
        unreleasedLinkLine =
            findLineWithUnreleasedLink 0 lines
    in
    Rule.errorForExtraFileWithFix
        fileKey
        { message = "Missing entry in CHANGELOG.md for version " ++ elmJsonData.version
        , details = [ "It seems you have or are ready to release a new version of your package, but forgot to include releases notes for it in your CHANGELOG.md file." ]
        }
        (case unreleased of
            Just ( lineNumber, line ) ->
                { start = { row = lineNumber, column = 1 }, end = { row = lineNumber, column = String.length line + 1 } }

            Nothing ->
                { start = { row = 1, column = 1 }, end = { row = 1, column = String.length (List.head lines |> Maybe.withDefault "") + 1 } }
        )
        (errorFix unreleased unreleasedLinkLine elmJsonData)


errorFix : Maybe ( Int, a ) -> Maybe Int -> ElmJsonData -> List Fix.Fix
errorFix unreleased unreleasedLinkLine elmJsonData =
    case unreleased of
        Just ( lineNumber, _ ) ->
            Fix.insertAt { row = lineNumber + 1, column = 1 } ("\n## [" ++ elmJsonData.version ++ "]\n\n")
                :: fixForUnreleasedLinkLine unreleasedLinkLine elmJsonData

        Nothing ->
            []


fixForUnreleasedLinkLine : Maybe Int -> ElmJsonData -> List Fix.Fix
fixForUnreleasedLinkLine unreleasedLinkLine { packageName, version } =
    case unreleasedLinkLine of
        Just lineNumber ->
            [ Fix.replaceRangeBy
                { start = { row = lineNumber, column = 1 }
                , end = { row = lineNumber + 1, column = 1 }
                }
                (("[Unreleased]: https://github.com/" ++ packageName ++ "/compare/v" ++ version ++ "...HEAD\n")
                    ++ ("[" ++ version ++ "]: https://github.com/" ++ packageName ++ "/releases/tag/" ++ version ++ "\n")
                )
            ]

        Nothing ->
            []


findLineWithUnreleased : Int -> List String -> Maybe ( Int, String )
findLineWithUnreleased index lines =
    case lines of
        [] ->
            Nothing

        line :: rest ->
            if String.contains "# [Unreleased]" line then
                Just ( index + 1, line )

            else
                findLineWithUnreleased (index + 1) rest


findLineWithUnreleasedLink : Int -> List String -> Maybe Int
findLineWithUnreleasedLink index lines =
    case lines of
        [] ->
            Nothing

        line :: rest ->
            if String.startsWith "[Unreleased]: " line then
                Just (index + 1)

            else
                findLineWithUnreleasedLink (index + 1) rest
