module SyntaxHelp2 exposing
    ( ExposedDefinitions
    , Link
    , LinkKind(..)
    , ModuleInfo
    , addLocation
    , docOfDeclaration
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
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import ParserExtra


docOfDeclaration : Declaration -> Maybe (Node Documentation)
docOfDeclaration declaration =
    case declaration of
        Declaration.FunctionDeclaration { documentation } ->
            documentation

        Declaration.AliasDeclaration { documentation } ->
            documentation

        Declaration.CustomTypeDeclaration { documentation } ->
            documentation

        Declaration.PortDeclaration _ ->
            Nothing

        Declaration.InfixDeclaration _ ->
            Nothing

        Declaration.Destructuring _ _ ->
            Nothing


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


{-| `moduleName = []` for links like `[`a`](#a)`.
-}
type alias Link =
    { moduleName : ModuleName
    , kind : LinkKind
    }


{-|

  - `DefinitionLink "a"` for links like `[`a`](#a)` or `[`a`](A#a)`.
  - `ModuleLink` for links like `[`A.And.B`](A-And-B)`.

-}
type LinkKind
    = ModuleLink
    | DefinitionLink String


nameParser : { first : Char -> Bool } -> Parser String
nameParser test =
    Parser.succeed ()
        |. Parser.chompIf
            (\c -> test.first c && Char.isAlphaNum c)
        |. Parser.chompWhile
            (\c -> Char.isAlphaNum c || c == '_')
        |> Parser.getChompedString


{-| See [`Link`](#Link).
-}
linkParser : Parser Link
linkParser =
    Parser.succeed
        (\moduleName kind ->
            { moduleName = moduleName, kind = kind }
        )
        |. Parser.Extras.brackets (Parser.chompUntil "]")
        |. Parser.symbol "("
        |= ParserExtra.manySeparated
            { by = "-"
            , item = nameParser { first = Char.isUpper }
            }
        |= Parser.oneOf
            [ Parser.succeed DefinitionLink
                |. Parser.symbol "#"
                |= nameParser { first = \_ -> True }
                |. Parser.symbol ")"
            , Parser.succeed ModuleLink
                |. Parser.token ")"
            ]
