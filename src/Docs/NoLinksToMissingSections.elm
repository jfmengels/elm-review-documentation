module Docs.NoLinksToMissingSections exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Markdown.Block
import Markdown.Parser
import ParserExtra
import Regex
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import SyntaxHelp2 as SyntaxHelp


{-| Reports... REPLACEME

    config =
        [ Docs.NoLinksToMissingSections.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.NoLinksToMissingSections
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.NoLinksToMissingSections" []
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = List.append
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    List FileLinksAndSections


type alias FileLinksAndSections =
    { moduleName : ModuleName
    , moduleKey : FileKey
    , sections : Set String
    , links : List (Node SyntaxHelp.Link)
    }


type FileKey
    = ModuleKey Rule.ModuleKey
    | ReadmeKey Rule.ReadmeKey


type alias ModuleContext =
    { exposingAll : Bool
    , moduleName : ModuleName
    , sections : Set String
    , links : List (Node SyntaxHelp.Link)
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleContext ->
            [ { moduleName = moduleContext.moduleName
              , moduleKey = ModuleKey moduleKey
              , sections = moduleContext.sections
              , links = moduleContext.links
              }
            ]
        )
        |> Rule.withModuleKey


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\metadata _ ->
            { exposingAll = False
            , moduleName = Rule.moduleNameFromMetadata metadata
            , sections = Set.empty
            , links = []
            }
        )
        |> Rule.withMetadata


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List nothing, ProjectContext )
readmeVisitor maybeReadmeInfo projectContext =
    case maybeReadmeInfo of
        Just { readmeKey, content } ->
            ( []
            , { moduleName = []
              , moduleKey = ReadmeKey readmeKey
              , sections = Set.fromList (extractSectionsFromHeadings content)
              , links = linksIn [] { row = 1, column = 1 } content
              }
                :: projectContext
            )

        Nothing ->
            ( [], projectContext )



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor node context =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            ( [], { context | exposingAll = True } )

        Exposing.Explicit exposed ->
            ( [], { context | sections = Set.fromList (List.map exposedName exposed) } )


exposedName : Node Exposing.TopLevelExpose -> String
exposedName node =
    case Node.value node of
        Exposing.InfixExpose string ->
            string

        Exposing.FunctionExpose string ->
            string

        Exposing.TypeOrAliasExpose string ->
            string

        Exposing.TypeExpose exposedType ->
            exposedType.name



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> ModuleContext -> ( List nothing, ModuleContext )
commentsVisitor comments context =
    let
        docs : List (Node String)
        docs =
            List.filter (Node.value >> String.startsWith "{-|") comments

        titleSections : List String
        titleSections =
            List.concatMap (Node.value >> extractSectionsFromHeadings) docs

        links : List (Node SyntaxHelp.Link)
        links =
            List.concatMap
                (\doc -> linksIn context.moduleName (Node.range doc).start (Node.value doc))
                docs
    in
    ( []
    , { exposingAll = context.exposingAll
      , moduleName = context.moduleName
      , sections = Set.union context.sections (Set.fromList titleSections)
      , links = links ++ context.links
      }
    )



-- DECLARATION VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations context =
    let
        docs : List (Node Documentation)
        docs =
            List.filterMap
                (Node.value >> docOfDeclaration)
                declarations

        declarationSections : List String
        declarationSections =
            if context.exposingAll then
                List.filterMap (Node.value >> nameOfDeclaration) declarations

            else
                []

        titleSections : List String
        titleSections =
            List.concatMap (Node.value >> extractSectionsFromHeadings) docs

        links : List (Node SyntaxHelp.Link)
        links =
            List.concatMap
                (\doc -> linksIn context.moduleName (Node.range doc).start (Node.value doc))
                docs
    in
    ( []
    , { exposingAll = context.exposingAll
      , moduleName = context.moduleName
      , sections = Set.union context.sections (Set.fromList (titleSections ++ declarationSections))
      , links = links ++ context.links
      }
    )


extractSectionsFromHeadings : String -> List String
extractSectionsFromHeadings string =
    Markdown.Parser.parse string
        |> Result.map extractTitles
        |> Result.withDefault []


toSlug : String -> String
toSlug string =
    string
        |> String.toLower
        |> Regex.replace specials (\_ -> "")
        |> Regex.replace whitespace (\_ -> "-")


specials : Regex.Regex
specials =
    "[\u{2000}-\u{206F}⸀-\u{2E7F}\\\\'!\"#$%&()*+,./:;<=>?@[\\\\]^`{|}~’]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


whitespace : Regex.Regex
whitespace =
    "\\s"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


extractTitles : List Markdown.Block.Block -> List String
extractTitles blocks =
    Markdown.Block.foldl
        (\block ( occurrencesTracker, acc ) ->
            case block of
                Markdown.Block.Heading _ inlines ->
                    let
                        rawSlug : String
                        rawSlug =
                            Markdown.Block.extractInlineText inlines
                                |> toSlug

                        ( finalSlug, updatedOccurrences ) =
                            trackOccurence rawSlug occurrencesTracker
                    in
                    ( updatedOccurrences
                    , finalSlug :: acc
                    )

                _ ->
                    ( occurrencesTracker, acc )
        )
        ( Dict.empty, [] )
        blocks
        |> Tuple.second


{-| Credit for this algorithm goes to
<https://github.com/Flet/github-slugger/blob/master/index.js>
TODO - this doesn't strip emoji yet
-}
trackOccurence : String -> Dict.Dict String Int -> ( String, Dict.Dict String Int )
trackOccurence slug occurences =
    case Dict.get slug occurences of
        Just n ->
            occurences
                |> increment slug
                |> trackOccurence (slug ++ "-" ++ String.fromInt n)

        Nothing ->
            ( slug, increment slug occurences )


increment : String -> Dict.Dict String Int -> Dict.Dict String Int
increment value occurences =
    Dict.update value
        (\maybeOccurence ->
            case maybeOccurence of
                Just count ->
                    Just <| count + 1

                Nothing ->
                    Just 1
        )
        occurences


nameOfDeclaration : Declaration -> Maybe String
nameOfDeclaration decl =
    case decl of
        Declaration.FunctionDeclaration { declaration } ->
            declaration
                |> Node.value
                |> .name
                |> Node.value
                |> Just

        Declaration.AliasDeclaration { name } ->
            Just (Node.value name)

        Declaration.CustomTypeDeclaration { name } ->
            Just (Node.value name)

        Declaration.PortDeclaration { name } ->
            Just (Node.value name)

        Declaration.InfixDeclaration { operator } ->
            Just (Node.value operator)

        Declaration.Destructuring _ _ ->
            Nothing


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


linksIn : ModuleName -> Location -> Documentation -> List (Node SyntaxHelp.Link)
linksIn currentModuleName offset documentation =
    documentation
        |> ParserExtra.find SyntaxHelp.linkParser
        |> List.filterMap identity
        |> List.map (normalizeModuleName currentModuleName >> addOffset offset)


normalizeModuleName : ModuleName -> Node SyntaxHelp.Link -> Node SyntaxHelp.Link
normalizeModuleName currentModuleName ((Node range link) as node) =
    case link.file of
        SyntaxHelp.ModuleTarget [] ->
            Node range { link | file = SyntaxHelp.ModuleTarget currentModuleName }

        SyntaxHelp.ModuleTarget _ ->
            node

        SyntaxHelp.ReadmeTarget ->
            node


addOffset : Location -> Node a -> Node a
addOffset offset (Node range a) =
    Node (SyntaxHelp.addOffset offset range) a



-- FINAL EVALUATION


finalEvaluation : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluation projectContext =
    let
        sectionsPerModule : Dict ModuleName (Set String)
        sectionsPerModule =
            projectContext
                |> List.map (\module_ -> ( module_.moduleName, module_.sections ))
                |> Dict.fromList
    in
    List.concatMap (errorsForFile sectionsPerModule) projectContext


errorsForFile : Dict ModuleName (Set String) -> FileLinksAndSections -> List (Rule.Error { useErrorForModule : () })
errorsForFile sectionsPerModule context =
    List.filterMap
        (isLinkToMissingSection sectionsPerModule context.moduleKey)
        context.links


isLinkToMissingSection : Dict ModuleName (Set String) -> FileKey -> Node SyntaxHelp.Link -> Maybe (Rule.Error scope)
isLinkToMissingSection sectionsPerModule fileKey ((Node _ link) as linkNode) =
    case link.file of
        SyntaxHelp.ModuleTarget moduleName ->
            case Dict.get moduleName sectionsPerModule of
                Just existingSections ->
                    case link.section of
                        Just section ->
                            if not (Set.member section existingSections) then
                                Just (reportLink fileKey linkNode)

                            else
                                Nothing

                        Nothing ->
                            Nothing

                Nothing ->
                    Just (reportUnknownModule fileKey moduleName linkNode)

        SyntaxHelp.ReadmeTarget ->
            case Dict.get [] sectionsPerModule of
                Just existingSections ->
                    case link.section of
                        Just section ->
                            if not (Set.member section existingSections) then
                                Just (reportLink fileKey linkNode)

                            else
                                Nothing

                        Nothing ->
                            Nothing

                Nothing ->
                    Just (reportLinkToMissingReadme fileKey linkNode)


reportLink : FileKey -> Node SyntaxHelp.Link -> Rule.Error scope
reportLink fileKey link =
    reportForFile fileKey
        { message = "Link points to a non-existing section or element"
        , details = [ "This is a dead link." ]
        }
        (Node.range link)


reportUnknownModule : FileKey -> ModuleName -> Node SyntaxHelp.Link -> Rule.Error scope
reportUnknownModule fileKey moduleName (Node range link) =
    reportForFile fileKey
        { message = "Link points to non-existing module " ++ String.join "." moduleName
        , details = [ "This is a dead link." ]
        }
        range


reportLinkToMissingReadme : FileKey -> Node SyntaxHelp.Link -> Rule.Error scope
reportLinkToMissingReadme fileKey (Node range link) =
    reportForFile fileKey
        { message = "Link points to missing README"
        , details = [ "elm-review only looks for a 'README.md' located next to your 'elm.json'. Maybe it's positioned elsewhere or named differently?" ]
        }
        range


reportForFile : FileKey -> { message : String, details : List String } -> Range -> Rule.Error scope
reportForFile fileKey =
    case fileKey of
        ModuleKey moduleKey ->
            Rule.errorForModule moduleKey

        ReadmeKey readmeKey ->
            Rule.errorForReadme readmeKey
