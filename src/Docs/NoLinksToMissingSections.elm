module Docs.NoLinksToMissingSections exposing (rule)

{-|

@docs rule

-}

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
    Rule.newModuleRuleSchema "Docs.NoLinksToMissingSections" initialContext
        |> moduleVisitor
        |> Rule.fromModuleRuleSchema


type alias ProjectContext =
    List
        { moduleName : ModuleName
        , moduleKey : Rule.ModuleKey
        , sections : Set String
        , links : List (Node SyntaxHelp.Link)
        }


type alias ModuleContext =
    { exposingAll : Bool
    , sections : Set String
    , links : List (Node SyntaxHelp.Link)
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleKey moduleContext ->
            [ { moduleName = Rule.moduleNameFromMetadata metadata
              , moduleKey = moduleKey
              , sections = moduleContext.sections
              , links = moduleContext.links
              }
            ]
        )
        |> Rule.withMetadata
        |> Rule.withModuleKey


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\_ ->
            { exposingAll = False
            , sections = Set.empty
            , links = []
            }
        )


initialContext : ModuleContext
initialContext =
    { exposingAll = False
    , sections = Set.empty
    , links = []
    }


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation



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
                (Node.value >> docOfDeclaration >> Maybe.map linksIn >> Maybe.withDefault [])
                declarations
    in
    ( []
    , { context
        | sections = Set.union context.sections (Set.fromList newSections)
        , links = links ++ context.links
      }
    )


linksToMissingSection : Set String -> Node SyntaxHelp.Link -> Bool
linksToMissingSection existingSections (Node _ link) =
    case link.section of
        Just section ->
            not (Set.member section existingSections)

        Nothing ->
            -- TODO
            False


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


linksIn : Node Documentation -> List (Node SyntaxHelp.Link)
linksIn documentation =
    Node.value documentation
        |> ParserExtra.find SyntaxHelp.linkParser
        |> List.map (mapNodeRange (SyntaxHelp.addOffset (Node.range documentation).start))


mapNodeRange : (Range -> Range) -> Node a -> Node a
mapNodeRange mapper (Node range a) =
    Node (mapper range) a



-- FINAL EVALUATION


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    context.links
        |> List.filter (linksToMissingSection context.sections)
        |> List.map reportLink


reportLink : Node SyntaxHelp.Link -> Rule.Error {}
reportLink link =
    Rule.error
        { message = "Link points to a non-existing section or element"
        , details = [ "This is a dead link." ]
        }
        (Node.range link)
