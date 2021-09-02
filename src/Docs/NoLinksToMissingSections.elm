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
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { sections : Set String
    }


initialContext : Context
initialContext =
    { sections = Set.empty
    }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor node context =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            ( [], context )

        Exposing.Explicit _ ->
            ( [], context )



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node context =
    case docOfDeclaration (Node.value node) of
        Just (Node range doc) ->
            let
                errors : List (Rule.Error {})
                errors =
                    linksIn { doc = doc, start = range.start }
                        |> List.filter (linksToMissingSection context.sections)
                        |> List.map reportLink
            in
            ( errors, context )

        Nothing ->
            ( [], context )


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


linksIn : { doc : String, start : Location } -> List (Node SyntaxHelp.Link)
linksIn documentation =
    documentation.doc
        |> ParserExtra.find SyntaxHelp.linkParser
        |> List.map (mapNodeRange (SyntaxHelp.addOffset documentation.start))


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
