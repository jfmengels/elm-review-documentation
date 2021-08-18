module LinksPointToExistingPackageMembers exposing (rule)

{-|

@docs rule

-}

import LinksPointToExistingPackageMembers.NotExposed
import Review.Rule exposing (Rule)


{-| Reports links to nonexistent package definitions or modules.

    config =
        [ LinksPointToExistingPackageMembers.rule
        ]


## Fails

    module A exposing (a)

    b =
        "b"

    {-| Not [`b`](A#b).
    -}
    a =
        "a"

Fails because `b` must be exposed.


## Success

    module A.And.B exposing (a, b)

    b =
        "b"

    {-| Not [`b`](A-And-B#b).
    -}
    a =
        "a"


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template lue-bird/elm-review-links-point-to-existing-package-members/example --rules LinksPointToExistingPackageMembers
```

-}
rule : Rule
rule =
    LinksPointToExistingPackageMembers.NotExposed.rule
