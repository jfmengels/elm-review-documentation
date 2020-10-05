module Documentation.NoMissing exposing
    ( rule
    , What, everything, onlyExposed
    , From, allModules, exposedModules
    )

{-|

@docs rule
@docs What, everything, onlyExposed
@docs From, allModules, exposedModules


## When (not) to enable this rule

This rule is useful when you care about having a thoroughly documented project.
It is also useful when you write Elm packages, in order to know about missing documentation before you publish.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Documentation.NoMissing
```

-}

import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports missing documentation for functions and types

    config =
        [ Documentation.NoMissing.rule
            { document = Documentation.NoMissing.everything
            , from = Documentation.NoMissing.exposedModules
            }
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

-}
rule : { document : What, from : From } -> Rule
rule configuration =
    Rule.newModuleRuleSchema "Documentation.NoMissing" initialContext
        |> Rule.withElmJsonModuleVisitor elmJsonVisitor
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor configuration.from)
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withDeclarationEnterVisitor (declarationVisitor configuration.document)
        |> Rule.fromModuleRuleSchema


type alias Context =
    { moduleNameRange : Range
    , exposedModules : Set String
    , exposedElements : Exposed
    , shouldBeReported : Bool
    }


initialContext : Context
initialContext =
    { moduleNameRange = Range.emptyRange
    , exposedModules = Set.empty
    , exposedElements = EverythingIsExposed
    , shouldBeReported = True
    }


type Exposed
    = EverythingIsExposed
    | ExplicitList (Set String)


{-| Which elements from a module should be documented.
-}
type What
    = Everything
    | OnlyExposed


{-| Everything function and type from a module should be documented. The module definition should also be documented.
-}
everything : What
everything =
    Everything


{-| Only exposed functions and types from a module should be documented. The module definition should also be documented.
-}
onlyExposed : What
onlyExposed =
    OnlyExposed


{-| Which modules should be documented.
-}
type From
    = AllModules
    | ExposedModules


{-| All modules from the project should be documented.

You might want to use [`ignoreErrorsForDirectories`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#ignoreErrorsForDirectories) to ignore the source directories.

-}
allModules : From
allModules =
    AllModules


{-| Only exposed modules from the project should be documented.

If your project is an application, you should not use this option. An application does not expose modules which would mean there are not modules to report.

You might want to use [`ignoreErrorsForDirectories`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#ignoreErrorsForDirectories) to ignore the source directories.

-}
exposedModules : From
exposedModules =
    ExposedModules



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe Elm.Project.Project -> Context -> Context
elmJsonVisitor maybeProject context =
    let
        exposedModules_ : Set String
        exposedModules_ =
            case maybeProject of
                Just (Elm.Project.Package package) ->
                    case package.exposed of
                        Elm.Project.ExposedList list ->
                            list
                                |> List.map Elm.Module.toString
                                |> Set.fromList

                        Elm.Project.ExposedDict list ->
                            list
                                |> List.concatMap Tuple.second
                                |> List.map Elm.Module.toString
                                |> Set.fromList

                _ ->
                    Set.empty
    in
    { context | exposedModules = exposedModules_ }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : From -> Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor fromConfig node context =
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
            case fromConfig of
                AllModules ->
                    True

                ExposedModules ->
                    Set.member (Node.value moduleName) context.exposedModules

        exposed : Exposed
        exposed =
            case Node.value node |> Module.exposingList of
                Exposing.All _ ->
                    EverythingIsExposed

                Exposing.Explicit list ->
                    ExplicitList (List.map collectExposing list |> Set.fromList)
    in
    ( []
    , { context
        | moduleNameRange = Node.range moduleName
        , shouldBeReported = shouldBeReported
        , exposedElements = exposed
      }
    )


collectExposing : Node Exposing.TopLevelExpose -> String
collectExposing node =
    case Node.value node of
        Exposing.InfixExpose name ->
            name

        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.TypeExpose exposedType ->
            exposedType.name



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> Context -> ( List (Error {}), Context )
commentsVisitor comments context =
    if context.shouldBeReported then
        let
            documentation : Maybe (Node String)
            documentation =
                findFirst (Node.value >> String.startsWith "{-|") comments
        in
        ( checkDocumentation documentation context.moduleNameRange
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



-- DECLARATION VISITOR


declarationVisitor : What -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor documentWhat node context =
    if context.shouldBeReported then
        ( case Node.value node of
            Declaration.FunctionDeclaration { documentation, declaration } ->
                let
                    (Node range name) =
                        (Node.value declaration).name
                in
                if shouldBeDocumented documentWhat context name then
                    checkDocumentation documentation range

                else
                    []

            Declaration.CustomTypeDeclaration { documentation, name } ->
                if shouldBeDocumented documentWhat context (Node.value name) then
                    checkDocumentation documentation (Node.range name)

                else
                    []

            Declaration.AliasDeclaration { documentation, name } ->
                if shouldBeDocumented documentWhat context (Node.value name) then
                    checkDocumentation documentation (Node.range name)

                else
                    []

            _ ->
                []
        , context
        )

    else
        ( [], context )


shouldBeDocumented : What -> Context -> String -> Bool
shouldBeDocumented documentWhat context name =
    case documentWhat of
        Everything ->
            True

        OnlyExposed ->
            case context.exposedElements of
                EverythingIsExposed ->
                    True

                ExplicitList exposedElements ->
                    Set.member name exposedElements


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
