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
        Declaration.FunctionDeclaration { declaration, documentation } ->
            case documentation of
                Just _ ->
                    []

                Nothing ->
                    [ Rule.error
                        { message = "REPLACEME"
                        , details = [ "REPLACEME" ]
                        }
                        (declaration |> Node.value |> .name |> Node.range)
                    ]

        _ ->
            []
