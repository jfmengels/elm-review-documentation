module Documentation.NoMissing exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports missing documentation for functions and types

    config =
        [ Documentation.NoMissing.rule
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
rule : Rule
rule =
    Rule.newModuleRuleSchema "Documentation.NoMissing" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { documentation, declaration } ->
            checkDocumentation documentation (Node.value declaration).name

        Declaration.CustomTypeDeclaration { documentation, name } ->
            checkDocumentation documentation name

        Declaration.AliasDeclaration { documentation, name } ->
            checkDocumentation documentation name

        _ ->
            []


checkDocumentation : Maybe (Node String) -> Node a -> List (Error {})
checkDocumentation documentation name =
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
                    (Node.range name)
                ]

            else
                []

        Nothing ->
            [ Rule.error
                { message = "Missing documentation"
                , details = [ "Documentation can help developers use this API." ]
                }
                (Node.range name)
            ]
