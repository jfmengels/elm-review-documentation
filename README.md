# elm-review-documentation

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to help with the quality and correctness of your Elm project's documentation.


## Provided rules

- [`Documentation.ReadmeLinksPointToCurrentVersion`](https://package.elm-lang.org/packages/jfmengels/elm-review-documentation/1.0.0/Documentation-ReadmeLinksPointToCurrentVersion) - Reports links in the `README.md` that do not point to the current version of the package.

## Configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import Documentation.ReadmeLinksPointToCurrentVersion

config : List Rule
config =
    [ Documentation.ReadmeLinksPointToCurrentVersion.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example
```
