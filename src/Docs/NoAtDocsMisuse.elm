module Docs.NoAtDocsMisuse exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Rule)



-- TODO Rename to Docs.ReviewAtDocs?


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
    { docsReferences : List (Node String)
    }


initialContext : Context
initialContext =
    { docsReferences =
        [ Node
            { start = { row = 4, column = 13 }
            , end = { row = 4, column = 20 }
            }
            "unknown"
        ]
    }



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> Context -> ( List nothing, Context )
commentsVisitor nodes context =
    case find (Node.value >> String.startsWith "{-|") nodes of
        Just moduleDocs ->
            ( []
            , context
            )

        Nothing ->
            ( [], context )



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> Context -> List (Rule.Error {})
declarationListVisitor nodes context =
    context.docsReferences
        |> List.map
            (\(Node range name) ->
                Rule.error
                    { message = "Found @docs reference for non-exposed `unknown`"
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
