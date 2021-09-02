module Docs.NoLinksToMissingSections exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import ParserExtra
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import SyntaxHelp2 as SyntaxHelp


{-| Reports... REPLACEME

    config =
        [ Docs.NoLinksToMissingSections.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.NoLinksToMissingSections
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.NoLinksToMissingSections" []
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = List.append
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    List CompiledModuleContext


type alias CompiledModuleContext =
    { moduleName : ModuleName
    , moduleKey : Rule.ModuleKey
    , sections : Set String
    , links : List (Node SyntaxHelp.Link)
    }


type alias ModuleContext =
    { exposingAll : Bool
    , moduleName : ModuleName
    , sections : Set String
    , links : List (Node SyntaxHelp.Link)
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleContext ->
            [ { moduleName = moduleContext.moduleName
              , moduleKey = moduleKey
              , sections = moduleContext.sections
              , links = moduleContext.links
              }
            ]
        )
        |> Rule.withModuleKey


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\metadata _ ->
            { exposingAll = False
            , moduleName = Rule.moduleNameFromMetadata metadata
            , sections = Set.empty
            , links = []
            }
        )
        |> Rule.withMetadata


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor node context =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            ( [], { context | exposingAll = True } )

        Exposing.Explicit exposed ->
            ( [], { context | sections = Set.fromList (List.map exposedName exposed) } )


exposedName : Node Exposing.TopLevelExpose -> String
exposedName node =
    case Node.value node of
        Exposing.InfixExpose string ->
            string

        Exposing.FunctionExpose string ->
            string

        Exposing.TypeOrAliasExpose string ->
            string

        Exposing.TypeExpose exposedType ->
            exposedType.name



-- DECLARATION VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations context =
    let
        newSections : List String
        newSections =
            if context.exposingAll then
                List.filterMap (Node.value >> nameOfDeclaration) declarations

            else
                []

        links : List (Node SyntaxHelp.Link)
        links =
            List.concatMap
                (Node.value >> docOfDeclaration >> Maybe.map (linksIn context.moduleName) >> Maybe.withDefault [])
                declarations
    in
    ( []
    , { exposingAll = context.exposingAll
      , moduleName = context.moduleName
      , sections = Set.union context.sections (Set.fromList newSections)
      , links = links ++ context.links
      }
    )


nameOfDeclaration : Declaration -> Maybe String
nameOfDeclaration decl =
    case decl of
        Declaration.FunctionDeclaration { declaration } ->
            declaration
                |> Node.value
                |> .name
                |> Node.value
                |> Just

        Declaration.AliasDeclaration { name } ->
            Just (Node.value name)

        Declaration.CustomTypeDeclaration { name } ->
            Just (Node.value name)

        Declaration.PortDeclaration { name } ->
            Just (Node.value name)

        Declaration.InfixDeclaration { operator } ->
            Just (Node.value operator)

        Declaration.Destructuring _ _ ->
            Nothing


docOfDeclaration : Declaration -> Maybe (Node Documentation)
docOfDeclaration declaration =
    case declaration of
        Declaration.FunctionDeclaration { documentation } ->
            documentation

        Declaration.AliasDeclaration { documentation } ->
            documentation

        Declaration.CustomTypeDeclaration { documentation } ->
            documentation

        Declaration.PortDeclaration _ ->
            Nothing

        Declaration.InfixDeclaration _ ->
            Nothing

        Declaration.Destructuring _ _ ->
            Nothing


type alias LinkWithRange =
    { parsed : SyntaxHelp.Link
    , range : Range
    }


linksIn : ModuleName -> Node Documentation -> List (Node SyntaxHelp.Link)
linksIn currentModuleName documentation =
    Node.value documentation
        |> ParserExtra.find SyntaxHelp.linkParser
        |> List.map (normalizeModuleName currentModuleName >> addOffset (Node.range documentation).start)


normalizeModuleName : ModuleName -> Node SyntaxHelp.Link -> Node SyntaxHelp.Link
normalizeModuleName currentModuleName ((Node range link) as node) =
    case link.file of
        SyntaxHelp.ModuleTarget [] ->
            Node range { link | file = SyntaxHelp.ModuleTarget currentModuleName }

        SyntaxHelp.ModuleTarget _ ->
            node

        SyntaxHelp.ReadmeTarget ->
            node


addOffset : Location -> Node a -> Node a
addOffset offset (Node range a) =
    Node (SyntaxHelp.addOffset offset range) a



-- FINAL EVALUATION


finalEvaluation : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluation projectContext =
    let
        sectionsPerModule : Dict ModuleName (Set String)
        sectionsPerModule =
            projectContext
                |> List.map (\module_ -> ( module_.moduleName, module_.sections ))
                |> Dict.fromList
    in
    List.concatMap (errorsForModule sectionsPerModule) projectContext


errorsForModule : Dict ModuleName (Set String) -> CompiledModuleContext -> List (Rule.Error { useErrorForModule : () })
errorsForModule sectionsPerModule context =
    List.filterMap
        (isLinkToMissingSection sectionsPerModule context.moduleKey)
        context.links


isLinkToMissingSection : Dict ModuleName (Set String) -> Rule.ModuleKey -> Node SyntaxHelp.Link -> Maybe (Rule.Error scope)
isLinkToMissingSection sectionsPerModule moduleKey ((Node _ link) as linkNode) =
    case link.file of
        SyntaxHelp.ModuleTarget moduleName ->
            case Dict.get moduleName sectionsPerModule of
                Just existingSections ->
                    case link.section of
                        Just section ->
                            if not (Set.member section existingSections) then
                                Just (reportLink moduleKey linkNode)

                            else
                                Nothing

                        Nothing ->
                            -- TODO
                            Nothing

                Nothing ->
                    Just (reportUnknownModule moduleKey moduleName linkNode)

        SyntaxHelp.ReadmeTarget ->
            -- TODO
            Nothing


reportLink : Rule.ModuleKey -> Node SyntaxHelp.Link -> Rule.Error scope
reportLink moduleKey link =
    Rule.errorForModule
        moduleKey
        { message = "Link points to a non-existing section or element"
        , details = [ "This is a dead link." ]
        }
        (Node.range link)


reportUnknownModule : Rule.ModuleKey -> ModuleName -> Node SyntaxHelp.Link -> Rule.Error scope
reportUnknownModule moduleKey moduleName (Node range link) =
    Rule.errorForModule
        moduleKey
        { message = "Link points to non-existing module " ++ String.join "." moduleName
        , details = [ "This is a dead link." ]
        }
        range
