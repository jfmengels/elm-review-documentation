module SyntaxHelp exposing (ExposingKind(..), Link, LinkKind(..), ModuleInfo, addLocation, docOfDeclaration, exposedModules, isExposed, isFileComment, linkParser, moduleInfo)

import Elm.Module as Module
import Elm.Project as Project
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import ParserExtra


docOfDeclaration : Declaration -> Maybe (Node Documentation)
docOfDeclaration declaration =
    case declaration of
        FunctionDeclaration { documentation } ->
            documentation

        AliasDeclaration { documentation } ->
            documentation

        CustomTypeDeclaration { documentation } ->
            documentation

        PortDeclaration _ ->
            Nothing

        InfixDeclaration _ ->
            Nothing

        Destructuring _ _ ->
            Nothing


type ExposingKind
    = All
    | Explicit


type alias ModuleInfo =
    { moduleName : List String
    , exposedDefinitions : ( ExposingKind, List String )
    }


moduleInfo : Module -> ModuleInfo
moduleInfo module_ =
    case module_ of
        NormalModule data ->
            extract data

        PortModule data ->
            extract data

        EffectModule data ->
            extract data


extract : { a | moduleName : Node ModuleName, exposingList : Node Exposing.Exposing } -> { moduleName : ModuleName, exposedDefinitions : ( ExposingKind, List String ) }
extract { moduleName, exposingList } =
    { moduleName = moduleName |> Node.value
    , exposedDefinitions =
        case exposingList |> Node.value of
            Exposing.All _ ->
                ( All, [] )

            Exposing.Explicit list ->
                ( Explicit
                , list
                    |> List.map (nameOfExpose << Node.value)
                )
    }


isExposed : String -> ( ExposingKind, List String ) -> Bool
isExposed definition exposings =
    case exposings of
        ( All, _ ) ->
            True

        ( Explicit, list ) ->
            list |> List.member definition


nameOfExpose : TopLevelExpose -> String
nameOfExpose expose =
    case expose of
        InfixExpose name ->
            name

        FunctionExpose name ->
            name

        TypeOrAliasExpose name ->
            name

        TypeExpose { name } ->
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
            fakeDict
                |> List.concatMap (\( _, names ) -> names)


addLocation : Location -> Location -> Location
addLocation aRange bRange =
    { row = aRange.row + bRange.row
    , column = aRange.column + bRange.column
    }


{-| `moduleName = []` for links like `[`a`](#a)`.
-}
type alias Link =
    { moduleName : List String
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
