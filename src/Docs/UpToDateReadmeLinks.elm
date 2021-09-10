module Docs.UpToDateReadmeLinks exposing (rule)

{-|

@docs rule

-}

import Docs.Utils.ParserExtra as ParserExtra
import Docs.Utils.SyntaxHelp as SyntaxHelp exposing (Link)
import Elm.Package
import Elm.Project
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Version
import Regex exposing (Regex)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports links in the `README.md` that point to this project's package documentation on <https://package.elm-lang.org/>,
where the version is set to `latest` or a different version than the current version of the package.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Docs.UpToDateReadmeLinks.rule
        ]

The problem with linking to `latest` is that if you release a new version later,
the users who read the README for the older version will be directed to a version
where the module/function/section you pointed to may not exist anymore.

This rule ensures that you always use the correct version in all of your releases,
and that you do not forget to update the links.

This rule provides automatic fixes, so you won't to do the tedious job of updating
the links yourself.

**NOTE**: Just make sure to run tests between bumping the version of the package
and publishing the package. Otherwise the link for a given version could link to a previous one.

**NOTE**: A similar rule would be useful for links inside the modules. I'll be working on that too!


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.UpToDateReadmeLinks
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.UpToDateReadmeLinks" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    Maybe
        { projectName : String
        , version : String
        }


initialProjectContext : ProjectContext
initialProjectContext =
    Nothing



-- elm.json VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject _ =
    case maybeProject |> Maybe.map .project of
        Just (Elm.Project.Package pkg) ->
            ( []
            , Just
                { projectName = Elm.Package.toString pkg.name
                , version = Elm.Version.toString pkg.version
                }
            )

        _ ->
            ( [], Nothing )



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List (Error scope), ProjectContext )
readmeVisitor maybeReadme maybeContext =
    case ( maybeReadme, maybeContext ) of
        ( Just { readmeKey, content }, Just context ) ->
            ( findRangeForSubstring context readmeKey content, maybeContext )

        _ ->
            ( [], maybeContext )


linkRegex : Regex
linkRegex =
    Regex.fromString "https://package\\.elm-lang\\.org/packages/([\\w-]+/[\\w-]+)/(\\w+(\\.\\w+\\.\\w+)?)(.*)"
        |> Maybe.withDefault Regex.never


findRangeForSubstring : { projectName : String, version : String } -> Rule.ReadmeKey -> String -> List (Error scope)
findRangeForSubstring context readmeKey content =
    content
        |> String.lines
        |> List.indexedMap Tuple.pair
        |> List.concatMap
            (\( row, lineContent ) ->
                lineContent
                    |> ParserExtra.find (SyntaxHelp.linkParser row [])
                    |> List.filterMap identity
                    |> List.concatMap
                        (\(Node range link) ->
                            reportError
                                context
                                readmeKey
                                range
                                link
                        )
            )


reportError : { projectName : String, version : String } -> Rule.ReadmeKey -> Range -> Link -> List (Error scope)
reportError context readmeKey range link =
    case link.file of
        SyntaxHelp.ModuleTarget moduleName ->
            [ Rule.errorForReadmeWithFix readmeKey
                { message = "Link does not point to the current version of the package"
                , details = [ "I suggest to run elm-review --fix to get the correct links." ]
                }
                range
                [ Fix.replaceRangeBy range <| "https://package.elm-lang.org/packages/" ++ context.projectName ++ "/" ++ context.version ++ "/" ++ String.join "-" moduleName ++ formatSlug link.slug ]
            ]

        SyntaxHelp.ReadmeTarget ->
            if link.startsWithDotSlash then
                [ Rule.errorForReadmeWithFix readmeKey
                    { message = "Link does not point to the current version of the package"
                    , details = [ "I suggest to run elm-review --fix to get the correct links." ]
                    }
                    range
                    (case link.slug of
                        Just slug ->
                            [ Fix.replaceRangeBy range ("#" ++ slug) ]

                        Nothing ->
                            []
                    )
                ]

            else
                []

        SyntaxHelp.External target ->
            Regex.find linkRegex target
                |> List.filterMap (notAMatch context readmeKey range)


formatSlug : Maybe String -> String
formatSlug maybeSlug =
    case maybeSlug of
        Just slug ->
            "#" ++ slug

        Nothing ->
            ""


notAMatch : { projectName : String, version : String } -> Rule.ReadmeKey -> Range -> Regex.Match -> Maybe (Error scope)
notAMatch { projectName, version } readmeKey range match =
    case match.submatches of
        (Just authorAndPackage) :: (Just linkVersion) :: _ :: rest :: [] ->
            if authorAndPackage == projectName && linkVersion /= version then
                Rule.errorForReadmeWithFix readmeKey
                    { message = "Link does not point to the current version of the package"
                    , details = [ "I suggest to run elm-review --fix to get the correct links." ]
                    }
                    range
                    [ Fix.replaceRangeBy range <| "https://package.elm-lang.org/packages/" ++ projectName ++ "/" ++ version ++ Maybe.withDefault "" rest ]
                    |> Just

            else
                Nothing

        _ ->
            Nothing
