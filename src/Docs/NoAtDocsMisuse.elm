module Docs.NoAtDocsMisuse exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Node exposing (Node)
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
    Rule.newModuleRuleSchema "Docs.NoAtDocsMisuse" ()
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    ()


commentsVisitor : List (Node String) -> Context -> ( List (Rule.Error {}), Context )
commentsVisitor nodes context =
    ( [ Rule.error
            { message = "Found @docs reference for non-exposed `unknown`"
            , details = [ "REPLACEME" ]
            }
            { start = { row = 4, column = 13 }
            , end = { row = 4, column = 20 }
            }
      ]
    , context
    )
