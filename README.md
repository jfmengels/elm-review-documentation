# elm-review-documentation

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to help with the quality and correctness of your Elm project's documentation.


## Provided rules

- [`Docs.NoAtDocsMisuse`](https://package.elm-lang.org/packages/jfmengels/elm-review-documentation/1.0.3/Docs-NoAtDocsMisuse) - Reports REPLACEME.
- [`Docs.NoMissing`](https://package.elm-lang.org/packages/jfmengels/elm-review-documentation/1.0.3/Docs-NoMissing) - Reports missing documentation for functions and types.
- [`Docs.ReviewLinksAndSections`](https://package.elm-lang.org/packages/jfmengels/elm-review-documentation/1.0.3/Docs-ReviewLinksAndSections) - Reports problems with links and sections in Elm projects.
- [🔧 `Docs.UpToDateReadmeLinks`](https://package.elm-lang.org/packages/jfmengels/elm-review-documentation/1.0.3/Docs-UpToDateReadmeLinks "Provides automatic fixes") - Reports links in the `README.md` that do not point to the current version of the package.

## Configuration

```elm
module ReviewConfig exposing (config)

import Docs.NoAtDocsMisuse
import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ Docs.NoMissing.rule
    , Docs.NoAtDocsMisuse.rule
        { document = onlyExposed
        , from = exposedModules
        }
    , Docs.UpToDateReadmeLinks.rule
    , Docs.ReviewLinksAndSections.rule
    ]
```

## Advice for package authors

You can ask the compiler to check whether the documentation is valid (as in, in a publishable state) if you're developing a package, by running `elm make --docs=docs.json`.
It will fail the compilation if documentation is missing, so you might not be able to enable it if the project is
far from ready yet. I do recommend enabling this as soon as you can, so that you can work on the documentation as you are
working on the project, not right before releasing when it might feel like a chore.

Additionally, running the previous command generates a `docs.json` file which I recommend you commit into the project, as
that will allow people to look at the documentation using [`elm-doc-preview`](https://elm-doc-preview.netlify.app/) and
give you feedback before you release.

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example
```

## Thanks

Thanks to @lue-bird for helping out with [`Docs.ReviewLinksAndSections`](https://package.elm-lang.org/packages/jfmengels/elm-review-documentation/1.0.3/Docs-ReviewLinksAndSections).
