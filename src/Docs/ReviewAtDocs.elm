module Docs.ReviewAtDocs exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Parser exposing ((|.), (|=), Parser)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)



-- TODO Report errors for @docs references in declaration comments
-- TODO Report errors for duplicate @docs references


{-| Reports... REPLACEME

    config =
        [ Docs.ReviewAtDocs.rule
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
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.ReviewAtDocs
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Docs.ReviewAtDocs" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withDeclarationListVisitor (\nodes context -> ( declarationListVisitor nodes context, context ))
        |> Rule.fromModuleRuleSchema


type alias Context =
    { exposed : Exposing
    , docsReferences : List (Node String)
    }


initialContext : Context
initialContext =
    { exposed = Exposing.All Range.emptyRange
    , docsReferences = []
    }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor node context =
    ( [], { context | exposed = Module.exposingList (Node.value node) } )



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> Context -> ( List (Rule.Error {}), Context )
commentsVisitor nodes context =
    case find (Node.value >> String.startsWith "{-|") nodes of
        Just (Node range comment) ->
            case String.lines comment of
                firstLine :: restOfLines ->
                    let
                        ( linesThatStartWithAtDocs, linesThatDontStartWithAtDocs ) =
                            restOfLines
                                |> List.indexedMap (\index line -> ( index + range.start.row + 1, line ))
                                |> List.partition (Tuple.second >> String.startsWith "@docs ")
                    in
                    ( List.append
                        (reportDocsOnFirstLine range.start.row firstLine)
                        (List.concatMap reportIndentedDocs linesThatDontStartWithAtDocs)
                    , { context | docsReferences = List.concatMap collectDocStatements linesThatStartWithAtDocs }
                    )

                [] ->
                    ( [], context )

        Nothing ->
            ( [], context )


reportDocsOnFirstLine : Int -> String -> List (Rule.Error {})
reportDocsOnFirstLine lineNumber line =
    Parser.run (Parser.succeed identity |. Parser.keyword "{-|" |= docsWithSpacesParser lineNumber) line
        |> Result.map
            (\range ->
                [ Rule.error
                    { message = "Found @docs on the first line"
                    , details = [ "Using @docs on the first line will make for a broken documentation once published. Please move it to the beginning of the next line." ]
                    }
                    range
                ]
            )
        |> Result.withDefault []


reportIndentedDocs : ( Int, String ) -> List (Rule.Error {})
reportIndentedDocs ( lineNumber, line ) =
    Parser.run (docsWithSpacesParser lineNumber) line
        |> Result.map
            (\range ->
                [ Rule.error
                    { message = "Found indented @docs"
                    , details = [ "@docs need to be at the beginning of a line, otherwise they can lead to broken documentation once published. on the first line will make for a broken documentation once published. Please remove the leading spaces" ]
                    }
                    range
                ]
            )
        |> Result.withDefault []


docsWithSpacesParser : Int -> Parser Range
docsWithSpacesParser row =
    Parser.succeed
        (\startColumn endColumn ->
            { start = { row = row, column = startColumn }, end = { row = row, column = endColumn } }
        )
        |. Parser.spaces
        |= Parser.getCol
        |. Parser.keyword "@docs"
        |= Parser.getCol


collectDocStatements : ( Int, String ) -> List (Node String)
collectDocStatements ( lineNumber, string ) =
    Parser.run (docElementsParser lineNumber) string
        |> Result.withDefault []


docElementsParser : Int -> Parser (List (Node String))
docElementsParser startRow =
    Parser.succeed identity
        |. Parser.keyword "@docs"
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ","
            , end = ""
            , spaces = Parser.spaces
            , item = docsItemParser startRow
            , trailing = Parser.Forbidden
            }


docsItemParser : Int -> Parser (Node String)
docsItemParser row =
    Parser.succeed
        (\startColumn name endColumn ->
            Node
                { start = { row = row, column = startColumn }
                , end = { row = row, column = endColumn }
                }
                name
        )
        |= Parser.getCol
        |= Parser.variable
            { start = Char.isAlpha
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.empty
            }
        |= Parser.getCol



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> Context -> List (Rule.Error {})
declarationListVisitor nodes context =
    let
        exposed : Set String
        exposed =
            case context.exposed of
                Exposing.All _ ->
                    List.filterMap declarationName nodes
                        |> Set.fromList

                Exposing.Explicit explicit ->
                    List.map topLevelExposeName explicit
                        |> Set.fromList
    in
    context.docsReferences
        |> List.filter (\(Node _ name) -> not (Set.member name exposed))
        |> List.map
            (\(Node range name) ->
                Rule.error
                    { message = "Found @docs reference for non-exposed `" ++ name ++ "`"
                    , details = [ "REPLACEME" ]
                    }
                    range
            )


declarationName : Node Declaration -> Maybe String
declarationName node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            function.declaration |> Node.value |> .name |> Node.value |> Just

        Declaration.AliasDeclaration typeAlias ->
            Just (Node.value typeAlias.name)

        Declaration.CustomTypeDeclaration type_ ->
            Just (Node.value type_.name)

        Declaration.PortDeclaration signature ->
            signature.name |> Node.value |> Just

        Declaration.InfixDeclaration { operator } ->
            Just (Node.value operator)

        Declaration.Destructuring _ _ ->
            Nothing


topLevelExposeName : Node Exposing.TopLevelExpose -> String
topLevelExposeName node =
    case Node.value node of
        Exposing.InfixExpose name ->
            name

        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.TypeExpose { name } ->
            name


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


indexedConcatMap : (Int -> a -> List b) -> List a -> List b
indexedConcatMap function list =
    List.foldr
        (\a ( index, acc ) -> ( index + 1, List.append (function index a) acc ))
        ( 0, [] )
        list
        |> Tuple.second
