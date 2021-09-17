module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.ReviewAtDocs
import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ Docs.NoMissing.rule
    , Docs.ReviewAtDocs.rule
        { document = onlyExposed
        , from = exposedModules
        }
    , Docs.UpToDateReadmeLinks.rule
    , Docs.ReviewLinksAndSections.rule
    ]
