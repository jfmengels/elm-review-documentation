module SyntaxHelp2 exposing
    ( FileTarget(..)
    , Link
    , addOffset
    , linkParser
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import ParserExtra


addLocation : Location -> Location -> Location
addLocation aRange bRange =
    { row = aRange.row + bRange.row
    , column = aRange.column + bRange.column
    }


addOffset : Location -> Range -> Range
addOffset offset { start, end } =
    { start = addLocation offset start
    , end = addLocation offset end
    }


{-| `moduleName = []` for links like `[`a`](#a)`.
-}
type alias Link =
    { file : FileTarget
    , section : Maybe String
    }


type FileTarget
    = ModuleTarget ModuleName
    | ReadmeTarget


idParser : Char -> Parser String
idParser endChar =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= endChar)
        |> Parser.getChompedString


moduleNameParser : Parser String
moduleNameParser =
    Parser.succeed ()
        |. Parser.chompIf (\c -> Char.isUpper c)
        |. Parser.chompWhile (\c -> Char.isAlphaNum c)
        |> Parser.getChompedString


linkParser : Parser (Maybe (Node Link))
linkParser =
    Parser.succeed identity
        |= Parser.getCol
        |. Parser.Extras.brackets (Parser.chompUntil "]")
        |> Parser.andThen
            (\col ->
                if col == 1 then
                    Parser.oneOf
                        [ inlineLinkParser
                            |> Parser.map Just
                        , shortcutLinkParser
                            |> Parser.map Just
                        , Parser.succeed Nothing
                        ]

                else
                    Parser.oneOf
                        [ Parser.map Just inlineLinkParser
                        , Parser.succeed Nothing
                        ]
            )


{-| Parses things like:

    This is a [link](#Link).

-}
inlineLinkParser : Parser (Node Link)
inlineLinkParser =
    Parser.succeed
        (\( startRow, startCol ) link ( endRow, endCol ) ->
            Node
                { start = { row = startRow - 1, column = startCol - 2 }
                , end = { row = endRow - 1, column = endCol - 2 }
                }
                link
        )
        |. Parser.symbol "("
        |= Parser.getPosition
        |= pathParser ')'
        |= Parser.getPosition
        |. Parser.token ")"


{-| Parses things like:

    This is a [link].

    [link]: #Link

-}
shortcutLinkParser : Parser (Node Link)
shortcutLinkParser =
    Parser.succeed
        (\( startRow, startCol ) link ( endRow, endCol ) ->
            Node
                { start = { row = startRow - 1, column = startCol - 2 }
                , end = { row = endRow - 1, column = endCol - 2 }
                }
                link
        )
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.getPosition
        |= pathParser '\n'
        |= Parser.getPosition


pathParser : Char -> Parser Link
pathParser endChar =
    Parser.oneOf
        [ Parser.succeed
            (\section ->
                { file = ModuleTarget [], section = Just section }
            )
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed
            (\file section ->
                { file = file, section = section }
            )
            |. ignoreDotSlash
            |= parseModuleName
            |= optionalSectionParser endChar
        ]


optionalSectionParser : Char -> Parser (Maybe String)
optionalSectionParser endChar =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed Nothing
        ]


parseModuleName : Parser FileTarget
parseModuleName =
    ParserExtra.manySeparated
        { by = "-"
        , item = moduleNameParser
        }
        |> Parser.map
            (\moduleName ->
                if List.isEmpty moduleName then
                    ReadmeTarget

                else
                    ModuleTarget moduleName
            )


ignoreDotSlash : Parser ()
ignoreDotSlash =
    Parser.oneOf
        [ Parser.symbol "."
            |. Parser.symbol "/"
        , Parser.succeed ()
        ]
