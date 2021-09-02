module Docs.NoLinksToMissingSections exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import ParserExtra
import Review.Rule as Rule exposing (Rule)
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
    Rule.newModuleRuleSchema "Docs.NoLinksToMissingSections" ()
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    ()


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node context =
    case docOfDeclaration (Node.value node) of
        Just (Node range doc) ->
            let
                errors : List (Rule.Error {})
                errors =
                    linksIn { doc = doc, start = range.start }
                        |> List.filter linksToMissingSection
                        |> List.map reportLink
            in
            ( errors, context )

        Nothing ->
            ( [], context )


linksToMissingSection : Node SyntaxHelp.Link -> Bool
linksToMissingSection node =
    True


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
