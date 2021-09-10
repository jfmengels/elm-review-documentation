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
    , startsWithDotSlash : Bool
    , slug : Maybe String
    }


type FileTarget
    = ModuleTarget ModuleName
    | ReadmeTarget
    | External String


idParser : Char -> Parser String
idParser endChar =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= endChar)
        |> Parser.getChompedString


onlyModuleNameParser : Parser ModuleName
onlyModuleNameParser =
    Parser.succeed identity
        |= moduleNameParser
        |. Parser.end


moduleNameParser : Parser ModuleName
moduleNameParser =
    ParserExtra.manySeparated
        { by = "-"
        , item = moduleNameSegmentParser
        }


moduleNameSegmentParser : Parser String
moduleNameSegmentParser =
    Parser.succeed ()
        |. Parser.chompIf (\c -> Char.isUpper c)
        |. Parser.chompWhile (\c -> Char.isAlphaNum c)
        |> Parser.getChompedString


linkParser : ModuleName -> Parser (Maybe (Node Link))
linkParser moduleName =
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
        |> Parser.map (Maybe.map (normalizeModuleName moduleName))


normalizeModuleName : ModuleName -> Node Link -> Node Link
normalizeModuleName currentModuleName ((Node range link) as node) =
    case link.file of
        ModuleTarget [] ->
            let
                file : FileTarget
                file =
                    if List.isEmpty currentModuleName then
                        ReadmeTarget

                    else
                        ModuleTarget currentModuleName
            in
            Node range { link | file = file }

        ModuleTarget _ ->
            node

        ReadmeTarget ->
            node

        External _ ->
            node


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
                { file = ModuleTarget [], startsWithDotSlash = False, slug = Just section }
            )
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed (\startsWithDotSlash file slug -> { file = file, startsWithDotSlash = startsWithDotSlash, slug = slug })
            |= ignoreDotSlash
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
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= '#' && c /= ')')
        |> Parser.getChompedString
        |> Parser.map
            (\linkTarget ->
                if linkTarget == "" then
                    ReadmeTarget

                else
                    case Parser.run onlyModuleNameParser linkTarget of
                        Ok moduleName ->
                            ModuleTarget moduleName

                        Err _ ->
                            External linkTarget
            )


ignoreDotSlash : Parser Bool
ignoreDotSlash =
    Parser.oneOf
        [ Parser.symbol "."
            |. Parser.symbol "/"
            |> Parser.map (\_ -> True)
        , Parser.succeed False
        ]


bracketsParser : Parser ()
bracketsParser =
    Parser.succeed identity
        |. Parser.symbol "["
        |. Parser.spaces
        |= Parser.chompUntil "]"
        |. Parser.spaces
        |. Parser.symbol "]"
