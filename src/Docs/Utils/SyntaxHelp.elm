module Docs.Utils.SyntaxHelp exposing
    ( FileTarget(..)
    , Link
    , addOffset
    , linkParser
    )

import Docs.Utils.ParserExtra as ParserExtra
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser exposing ((|.), (|=), Parser)


addOffset : Location -> Int -> Range -> Range
addOffset offset lineNumber { start, end } =
    { start = addLocation lineNumber offset start
    , end = addLocation lineNumber offset end
    }


addLocation : Int -> Location -> Location -> Location
addLocation lineNumber aRange bRange =
    { row = lineNumber + aRange.row + bRange.row
    , column = aRange.column + bRange.column
    }


type alias Link =
    { file : FileTarget
    , slug : Maybe String
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
        |. bracketsParser
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
                { file = ModuleTarget [], slug = Just section }
            )
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed
            (\file slug ->
                { file = file, slug = slug }
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


bracketsParser : Parser ()
bracketsParser =
    Parser.succeed identity
        |. Parser.symbol "["
        |. Parser.spaces
        |= Parser.chompUntil "]"
        |. Parser.spaces
        |. Parser.symbol "]"
