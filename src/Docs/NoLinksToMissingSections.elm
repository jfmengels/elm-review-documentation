module Docs.NoLinksToMissingSections exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
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
import SyntaxHelp


{-| Reports problems with links and sections in Elm projects.

    config =
        [ Docs.NoLinksToMissingSections.rule
        ]


## Fail

Links to missing modules or sections are reported.

    {-| Link to [missing module](Unknown-Module).
    -}
    a =
        1

    {-| Link to [missing section](#unknown).
    -}
    a =
        1

In packages, links that would appear in the public documentation and that link to sections not part of the public documentation are reported.

    module Exposed exposing (a)

    import Internal

    {-| Link to [internal details](Internal#section).
    -}
    a =
        1


## Success

    module Exposed exposing (a, b)

    import Internal

    {-| Link to [exposed b](#b).
    -}
    a =
        1

    b =
        2


## When (not) to enable this rule

For packages, this rule will be useful to prevent having dead links in the package documentation.

For applications, this rule will be useful if you have the habit of writing documentation the way you do in Elm packages,
and want to prevent it from going out of date.

This rule will not be useful if your project is an application and no-one in the team has the habit of writing
package-like documentation.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.NoLinksToMissingSections
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.NoLinksToMissingSections" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { fileLinksAndSections : List FileLinksAndSections
    , exposedModules : Set ModuleName
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { fileLinksAndSections = []
    , exposedModules = Set.empty
    }


type alias FileLinksAndSections =
    { moduleName : ModuleName
    , fileKey : FileKey
    , sections : List Section
    , links : List MaybeExposedLink
    }


type FileKey
    = ModuleKey Rule.ModuleKey
    | ReadmeKey Rule.ReadmeKey


type alias ModuleContext =
    { isModuleExposed : Bool
    , exposedElements : Set String
    , moduleName : ModuleName
    , sections : List Section
    , links : List MaybeExposedLink
    }


type alias Section =
    { slug : String
    , isExposed : Bool
    }


type MaybeExposedLink
    = MaybeExposedLink
        { link : SyntaxHelp.Link
        , linkRange : Range
        , isExposed : Bool
        }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\metadata projectContext ->
            let
                moduleName : ModuleName
                moduleName =
                    Rule.moduleNameFromMetadata metadata
            in
            { isModuleExposed = Set.member moduleName projectContext.exposedModules
            , exposedElements = Set.empty
            , moduleName = moduleName
            , sections = []
            , links = []
            }
        )
        |> Rule.withMetadata


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleContext ->
            { fileLinksAndSections =
                [ { moduleName = moduleContext.moduleName
                  , fileKey = ModuleKey moduleKey
                  , sections = moduleContext.sections
                  , links = moduleContext.links
                  }
                ]
            , exposedModules = Set.empty
            }
        )
        |> Rule.withModuleKey


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { fileLinksAndSections = List.append newContext.fileLinksAndSections previousContext.fileLinksAndSections
    , exposedModules = previousContext.exposedModules
    }


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson projectContext =
    case Maybe.map .project maybeElmJson of
        Just (Elm.Project.Package { exposed }) ->
            ( [], { projectContext | exposedModules = listExposedModules exposed } )

        _ ->
            ( [], projectContext )


listExposedModules : Elm.Project.Exposed -> Set ModuleName
listExposedModules exposed =
    let
        exposedModules : List ModuleName
        exposedModules =
            exposedModulesFromPackageAsList exposed
                |> List.map (Elm.Module.toString >> String.split ".")
    in
    Set.fromList ([] :: exposedModules)


exposedModulesFromPackageAsList : Elm.Project.Exposed -> List Elm.Module.Name
exposedModulesFromPackageAsList exposed =
    case exposed of
        Elm.Project.ExposedList list ->
            list

        Elm.Project.ExposedDict list ->
            List.concatMap Tuple.second list



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List nothing, ProjectContext )
readmeVisitor maybeReadmeInfo projectContext =
    case maybeReadmeInfo of
        Just { readmeKey, content } ->
            let
                isReadmeExposed : Bool
                isReadmeExposed =
                    Set.member [] projectContext.exposedModules
            in
            ( []
            , { fileLinksAndSections =
                    { moduleName = []
                    , fileKey = ReadmeKey readmeKey
                    , sections =
                        extractSlugsFromHeadings content
                            |> List.map (\slug -> { slug = slug, isExposed = isReadmeExposed })
                    , links =
                        linksIn [] { row = 1, column = 1 } content
                            |> List.map
                                (\link ->
                                    MaybeExposedLink
                                        { link = Node.value link
                                        , linkRange = Node.range link
                                        , isExposed = isReadmeExposed
                                        }
                                )
                    }
                        :: projectContext.fileLinksAndSections
              , exposedModules = projectContext.exposedModules
              }
            )

        Nothing ->
            ( [], projectContext )



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor node context =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            -- We'll keep `exposedElements` empty, which will make `declarationListVisitor` fill it with the known
            -- declarations.
            ( [], context )

        Exposing.Explicit exposed ->
            ( [], { context | exposedElements = Set.fromList (List.map exposedName exposed) } )


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

        sectionsAndLinks : List { titleSections : List Section, links : List MaybeExposedLink }
        sectionsAndLinks =
            List.map
                (findSectionsAndLinks
                    context.moduleName
                    context.isModuleExposed
                )
                docs
    in
    ( []
    , { isModuleExposed = context.isModuleExposed
      , exposedElements = context.exposedElements
      , moduleName = context.moduleName
      , sections = List.append (List.concatMap .titleSections sectionsAndLinks) context.sections
      , links = List.append (List.concatMap .links sectionsAndLinks) context.links
      }
    )



-- DECLARATION VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations context =
    let
        exposedElements : Set String
        exposedElements =
            if Set.isEmpty context.exposedElements then
                Set.fromList (List.filterMap nameOfDeclaration declarations)

            else
                context.exposedElements

        sectionsAndLinks : List { titleSections : List Section, links : List MaybeExposedLink }
        sectionsAndLinks =
            List.map
                (findSectionsAndLinksForDeclaration
                    context.moduleName
                    (if context.isModuleExposed then
                        exposedElements

                     else
                        Set.empty
                    )
                )
                declarations
    in
    ( []
    , { isModuleExposed = context.isModuleExposed
      , exposedElements = context.exposedElements
      , moduleName = context.moduleName
      , sections =
            List.concat
                [ List.concatMap .titleSections sectionsAndLinks
                , List.map (\slug -> { slug = slug, isExposed = True }) (Set.toList exposedElements)
                , context.sections
                ]
      , links = List.append (List.concatMap .links sectionsAndLinks) context.links
      }
    )


extractSlugsFromHeadings : String -> List String
extractSlugsFromHeadings string =
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


nameOfDeclaration : Node Declaration -> Maybe String
nameOfDeclaration node =
    case Node.value node of
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


findSectionsAndLinksForDeclaration : ModuleName -> Set String -> Node Declaration -> { titleSections : List Section, links : List MaybeExposedLink }
findSectionsAndLinksForDeclaration currentModuleName exposedElements declaration =
    case docOfDeclaration (Node.value declaration) of
        Just doc ->
            let
                name : String
                name =
                    nameOfDeclaration declaration
                        |> Maybe.withDefault ""

                isExposed : Bool
                isExposed =
                    Set.member name exposedElements
            in
            findSectionsAndLinks currentModuleName isExposed doc

        Nothing ->
            { titleSections = [], links = [] }


findSectionsAndLinks : ModuleName -> Bool -> Node String -> { titleSections : List Section, links : List MaybeExposedLink }
findSectionsAndLinks currentModuleName isExposed doc =
    let
        titleSections : List Section
        titleSections =
            extractSlugsFromHeadings (Node.value doc)
                |> List.map (\slug -> { slug = slug, isExposed = isExposed })

        links : List MaybeExposedLink
        links =
            linksIn currentModuleName (Node.range doc).start (Node.value doc)
                |> List.map
                    (\link ->
                        MaybeExposedLink
                            { link = Node.value link
                            , linkRange = Node.range link
                            , isExposed = isExposed
                            }
                    )
    in
    { titleSections = titleSections
    , links = links
    }


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
        sectionsPerModule : Dict ModuleName (List Section)
        sectionsPerModule =
            projectContext.fileLinksAndSections
                |> List.map (\module_ -> ( module_.moduleName, module_.sections ))
                |> Dict.fromList
    in
    List.concatMap (errorsForFile projectContext.exposedModules sectionsPerModule) projectContext.fileLinksAndSections


errorsForFile : Set ModuleName -> Dict ModuleName (List Section) -> FileLinksAndSections -> List (Rule.Error scope)
errorsForFile exposedModules sectionsPerModule fileLinksAndSections =
    List.filterMap
        (isLinkToMissingSection exposedModules sectionsPerModule fileLinksAndSections)
        fileLinksAndSections.links


isLinkToMissingSection : Set ModuleName -> Dict ModuleName (List Section) -> FileLinksAndSections -> MaybeExposedLink -> Maybe (Rule.Error scope)
isLinkToMissingSection exposedModules sectionsPerModule fileLinksAndSections (MaybeExposedLink { link, linkRange, isExposed }) =
    case link.file of
        SyntaxHelp.ModuleTarget moduleName ->
            case Dict.get moduleName sectionsPerModule of
                Just existingSections ->
                    if Set.member fileLinksAndSections.moduleName exposedModules && not (Set.member moduleName exposedModules) then
                        Just (reportLinkToNonExposedModule fileLinksAndSections.fileKey linkRange)

                    else
                        reportIfMissingSection fileLinksAndSections.fileKey existingSections isExposed linkRange link

                Nothing ->
                    Just (reportUnknownModule fileLinksAndSections.fileKey moduleName linkRange)

        SyntaxHelp.ReadmeTarget ->
            case Dict.get [] sectionsPerModule of
                Just existingSections ->
                    reportIfMissingSection fileLinksAndSections.fileKey existingSections isExposed linkRange link

                Nothing ->
                    Just (reportLinkToMissingReadme fileLinksAndSections.fileKey linkRange)


reportIfMissingSection : FileKey -> List Section -> Bool -> Range -> SyntaxHelp.Link -> Maybe (Rule.Error scope)
reportIfMissingSection fileKey existingSectionsForTargetFile isExposed linkRange link =
    case link.slug of
        Just slug ->
            case find (\section -> section.slug == slug) existingSectionsForTargetFile of
                Just section ->
                    if isExposed && not section.isExposed then
                        Just (reportLinkToNonExposedSection fileKey linkRange)

                    else
                        Nothing

                Nothing ->
                    Just (reportLink fileKey linkRange)

        Nothing ->
            Nothing


reportLink : FileKey -> Range -> Rule.Error scope
reportLink fileKey range =
    reportForFile fileKey
        { message = "Link points to a non-existing section or element"
        , details = [ "This is a dead link." ]
        }
        range


reportLinkToNonExposedModule : FileKey -> Range -> Rule.Error scope
reportLinkToNonExposedModule fileKey range =
    reportForFile fileKey
        { message = "Link in public documentation points to non-exposed module"
        , details = [ "Users will not be able to follow the link." ]
        }
        range


reportLinkToNonExposedSection : FileKey -> Range -> Rule.Error scope
reportLinkToNonExposedSection fileKey range =
    reportForFile fileKey
        { message = "Link in public documentation points to non-exposed section"
        , details = [ "Users will not be able to follow the link." ]
        }
        range


reportUnknownModule : FileKey -> ModuleName -> Range -> Rule.Error scope
reportUnknownModule fileKey moduleName range =
    reportForFile fileKey
        { message = "Link points to non-existing module " ++ String.join "." moduleName
        , details = [ "This is a dead link." ]
        }
        range


reportLinkToMissingReadme : FileKey -> Range -> Rule.Error scope
reportLinkToMissingReadme fileKey range =
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


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest
