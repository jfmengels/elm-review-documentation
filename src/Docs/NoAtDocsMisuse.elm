module Docs.NoAtDocsMisuse exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Parser exposing ((|.), (|=), Parser)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)



-- TODO Rename to Docs.ReviewAtDocs?
-- TODO Report errors for @docs references on the first line
-- TODO Report errors for @docs references that don't begin at the beginning of a line
-- TODO Report errors for @docs references in declaration comments
-- TODO Report errors for duplicate @docs references


{-| Reports... REPLACEME

    config =
        [ Docs.NoAtDocsMisuse.rule
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
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.NoAtDocsMisuse
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Docs.NoAtDocsMisuse" initialContext
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



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> Context -> ( List nothing, Context )
commentsVisitor nodes context =
    case find (Node.value >> String.startsWith "{-|") nodes of
        Just moduleDocs ->
            ( []
            , { context
                | docsReferences =
                    moduleDocs
                        |> Node.value
                        |> String.lines
                        |> List.drop 1
                        |> indexedConcatMap collectDocStatements
              }
            )

        Nothing ->
            ( [], context )


collectDocStatements : Int -> String -> List (Node String)
collectDocStatements lineNumber string =
    Parser.run (docsParser lineNumber) string
        |> Result.withDefault []


docsParser : Int -> Parser (List (Node String))
docsParser startRow =
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
            Set.fromList [ "a", "b", "d", "T", "D" ]
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
