# elm-review-documentation

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to help with the quality and correctness of your Elm project's documentation.


## Provided rules

- [🔧 `Documentation.ReadmeLinksPointToCurrentVersion`](https://package.elm-lang.org/packages/jfmengels/elm-review-documentation/1.0.1/Documentation-ReadmeLinksPointToCurrentVersion "Provides automatic fixes") - Reports links in the `README.md` that do not point to the current version of the package.

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

## Advice for package authors

You can ask the compiler to check whether the documentation is valid (as in, in a publishable state) if you're developing a package, by running `elm make --docs=docs.json`.
It will fail the compilation if documentation is missing, so you might not be able to enable it during if the project is
far from ready yet. I do recommend enabling this as soon as you can, so that you can work on the documentation as you are
working on the project, not right before releasing when it becomes a chore.

Additionally, running the previous command generates a `docs.json` file which I recommend you commit into the project, as
that will allow people to look at the documentation using [`elm-doc-preview`](https://elm-doc-preview.netlify.app/) and
give you feedback before you release.

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example
```
