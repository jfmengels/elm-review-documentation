module Docs.NoLinksToMissingSections exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
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
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { exposingAll : Bool
    , sections : Set String
    , links : List (Node SyntaxHelp.Link)
    }


initialContext : Context
initialContext =
    { exposingAll = False
    , sections = Set.empty
    , links = []
    }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
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


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor declarations context =
    let
        newSections =
            if context.exposingAll then
                []

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


reportLink : Node SyntaxHelp.Link -> Rule.Error {}
reportLink link =
    Rule.error
        { message = "Link points to a non-existing section or element"
        , details = [ "This is a dead link." ]
        }
        (Node.range link)


mapNodeRange : (Range -> Range) -> Node a -> Node a
mapNodeRange mapper (Node range a) =
    Node (mapper range) a



-- FINAL EVALUATION


finalEvaluation : Context -> List (Rule.Error {})
finalEvaluation context =
    context.links
        |> List.filter (linksToMissingSection context.sections)
        |> List.map reportLink
