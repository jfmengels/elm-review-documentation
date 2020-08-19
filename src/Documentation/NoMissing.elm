module Documentation.NoMissing exposing
    ( rule
    , Configuration, everything, onlyExposed
    )

{-|

@docs rule
@docs Configuration, everything, onlyExposed

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports missing documentation for functions and types

    config =
        [ Documentation.NoMissing.rule Documentation.NoMissing.onlyExposed

        -- or
        , Documentation.NoMissing.rule Documentation.NoMissing.everything
        ]


## Fail

    someFunction =
        great Things

    {-| -}
    someOtherFunction =
        other (great Things)


## Success

    {-| someFunction does great things
    -}
    someFunction =
        great Things


## When (not) to enable this rule

This rule is useful when you care about having a thoroughly documented project.
It is also useful when you write Elm packages, in order to know about missing documentation before you publish.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Documentation.NoMissing
```

-}
rule : Configuration -> Rule
rule configuration =
    Rule.newModuleRuleSchema "Documentation.NoMissing" Range.emptyRange
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


type Configuration
    = Everything
    | OnlyExposed


everything : Configuration
everything =
    Everything


onlyExposed : Configuration
onlyExposed =
    OnlyExposed


type alias Context =
    Range


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor node _ =
    let
        range : Range
        range =
            case Node.value node of
                Module.NormalModule x ->
                    Node.range x.moduleName

                Module.PortModule x ->
                    Node.range x.moduleName

                Module.EffectModule x ->
                    Node.range x.moduleName
    in
    ( [], range )


commentsVisitor : List (Node String) -> Context -> ( List (Error {}), Context )
commentsVisitor comments range =
    let
        documentation : Maybe (Node String)
        documentation =
            comments
                |> List.filter (Node.value >> String.startsWith "{-|")
                |> List.head
    in
    ( checkDocumentation documentation range
    , range
    )


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { documentation, declaration } ->
            checkDocumentation
                documentation
                (Node.range (Node.value declaration).name)

        Declaration.CustomTypeDeclaration { documentation, name } ->
            checkDocumentation documentation (Node.range name)

        Declaration.AliasDeclaration { documentation, name } ->
            checkDocumentation documentation (Node.range name)

        _ ->
            []


checkDocumentation : Maybe (Node String) -> Range -> List (Error {})
checkDocumentation documentation range =
    case documentation of
        Just doc ->
            let
                trimmedDocumentation : String
                trimmedDocumentation =
                    doc
                        |> Node.value
                        |> String.dropLeft 3
                        |> String.dropRight 2
                        |> String.trim
            in
            if trimmedDocumentation == "" then
                [ Rule.error
                    { message = "The documentation is empty"
                    , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                    }
                    range
                ]

            else
                []

        Nothing ->
            [ Rule.error
                { message = "Missing documentation"
                , details = [ "Documentation can help developers use this API." ]
                }
                range
            ]
