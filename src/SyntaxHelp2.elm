module SyntaxHelp2 exposing
    ( ExposedDefinitions
    , FileTarget(..)
    , Link
    , LinkKind(..)
    , ModuleInfo
    , addLocation
    , addOffset
    , exposedModules
    , isExposed
    , isFileComment
    , linkParser
    , moduleInfo
    )

import Elm.Module as Module
import Elm.Project as Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import ParserExtra


type ExposedDefinitions
    = All
    | Explicit


type alias ModuleInfo =
    { moduleName : ModuleName
    , exposedDefinitions : ( ExposedDefinitions, List String )
    }


moduleInfo : Module -> ModuleInfo
moduleInfo module_ =
    case module_ of
        Module.NormalModule data ->
            extract data

        Module.PortModule data ->
            extract data

        Module.EffectModule data ->
            extract data


extract : { a | moduleName : Node ModuleName, exposingList : Node Exposing.Exposing } -> { moduleName : ModuleName, exposedDefinitions : ( ExposedDefinitions, List String ) }
extract { moduleName, exposingList } =
    { moduleName = moduleName |> Node.value
    , exposedDefinitions =
        case exposingList |> Node.value of
            Exposing.All _ ->
                ( All, [] )

            Exposing.Explicit list ->
                ( Explicit
                , List.map (Node.value >> nameOfExpose) list
                )
    }


isExposed : String -> ( ExposedDefinitions, List String ) -> Bool
isExposed definition exposings =
    case exposings of
        ( All, _ ) ->
            True

        ( Explicit, list ) ->
            List.member definition list


nameOfExpose : TopLevelExpose -> String
nameOfExpose expose =
    case expose of
        Exposing.InfixExpose name ->
            name

        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.TypeExpose { name } ->
            name


isFileComment : String -> Bool
isFileComment =
    String.startsWith "{-|"


exposedModules : Project.Exposed -> List Module.Name
exposedModules exposed =
    case exposed of
        Project.ExposedList exposedList ->
            exposedList

        Project.ExposedDict fakeDict ->
            List.concatMap (\( _, names ) -> names) fakeDict


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


{-|

  - `DefinitionLink "a"` for links like `[`a`](#a)` or `[`a`](A#a)`.
  - `ModuleLink` for links like `[`A.And.B`](A-And-B)`.

-}
type LinkKind
    = ModuleLink
    | DefinitionLink String


idParser : Parser String
idParser =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= ')')
        |> Parser.getChompedString


moduleNameParser : Parser String
moduleNameParser =
    Parser.succeed ()
        |. Parser.chompIf (\c -> Char.isUpper c)
        |. Parser.chompWhile (\c -> Char.isAlphaNum c)
        |> Parser.getChompedString


{-| See [`Link`](#Link).
-}
linkParser : Parser (Node Link)
linkParser =
    Parser.succeed
        (\( startRow, startCol ) link ( endRow, endCol ) ->
            Node
                { start = { row = startRow - 1, column = startCol - 2 }
                , end = { row = endRow - 1, column = endCol - 3 }
                }
                link
        )
        |. Parser.Extras.brackets (Parser.chompUntil "]")
        |. Parser.symbol "("
        |= Parser.getPosition
        |= pathParser
        |. Parser.token ")"
        |= Parser.getPosition


pathParser : Parser Link
pathParser =
    Parser.oneOf
        [ Parser.succeed
            (\section ->
                { file = ModuleTarget [], section = Just section }
            )
            |. Parser.symbol "#"
            |= idParser
        , Parser.succeed
            (\file section ->
                { file = file, section = section }
            )
            |. ignoreDotSlash
            |= parseModuleName
            |= optionalSectionParser
        ]


optionalSectionParser : Parser (Maybe String)
optionalSectionParser =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.symbol "#"
            |= idParser
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
