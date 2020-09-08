module Documentation.NoMissing exposing
    ( rule
    , Configuration, everything, onlyExposed
    )

{-|

@docs rule
@docs Configuration, everything, onlyExposed

-}

import Elm.Module as Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


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
    Rule.newModuleRuleSchema "Documentation.NoMissing" initialContext
        |> Rule.withElmJsonModuleVisitor elmJsonVisitor
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor configuration)
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { moduleName : Node String
    , exposedModules : Set String
    , shouldBeReported : Bool
    }


initialContext : Context
initialContext =
    { moduleName = Node Range.emptyRange ""
    , exposedModules = Set.empty
    , shouldBeReported = True
    }


type Configuration
    = Everything
    | OnlyExposed


everything : Configuration
everything =
    Everything


onlyExposed : Configuration
onlyExposed =
    OnlyExposed


elmJsonVisitor : Maybe Elm.Project.Project -> Context -> Context
elmJsonVisitor maybeProject context =
    let
        exposedModules : Set String
        exposedModules =
            case maybeProject of
                Just (Elm.Project.Package package) ->
                    case package.exposed of
                        Elm.Project.ExposedList list ->
                            list
                                |> List.map Module.toString
                                |> Set.fromList

                        Elm.Project.ExposedDict list ->
                            list
                                |> List.concatMap Tuple.second
                                |> List.map Module.toString
                                |> Set.fromList

                _ ->
                    Set.empty
    in
    { context | exposedModules = exposedModules }


moduleDefinitionVisitor : Configuration -> Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor configuration node context =
    let
        moduleName : Node String
        moduleName =
            case Node.value node of
                Module.NormalModule x ->
                    Node
                        (Node.range x.moduleName)
                        (Node.value x.moduleName |> String.join ".")

                Module.PortModule x ->
                    Node
                        (Node.range x.moduleName)
                        (Node.value x.moduleName |> String.join ".")

                Module.EffectModule x ->
                    Node
                        (Node.range x.moduleName)
                        (Node.value x.moduleName |> String.join ".")

        shouldBeReported : Bool
        shouldBeReported =
            case configuration of
                Everything ->
                    True

                OnlyExposed ->
                    Set.member (Node.value moduleName) context.exposedModules
    in
    ( []
    , { context
        | moduleName = moduleName
        , shouldBeReported = shouldBeReported
      }
    )


commentsVisitor : List (Node String) -> Context -> ( List (Error {}), Context )
commentsVisitor comments context =
    if context.shouldBeReported then
        let
            documentation : Maybe (Node String)
            documentation =
                findFirst (Node.value >> String.startsWith "{-|") comments
        in
        ( checkDocumentation documentation (Node.range context.moduleName)
        , context
        )

    else
        ( [], context )


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst predicate list =
    case list of
        [] ->
            Nothing

        a :: rest ->
            if predicate a then
                Just a

            else
                findFirst predicate rest


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor node context =
    if context.shouldBeReported then
        ( case Node.value node of
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
        , context
        )

    else
        ( [], context )


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
