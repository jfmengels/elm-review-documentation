module Docs.LinksPointToExistingPackageMembers exposing (rule)

import Dict exposing (Dict)
import Elm.Module as Module
import Elm.Project as Project exposing (Project)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import EverySet exposing (EverySet)
import JaroWinkler
import ParserExtra as Parser
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import SyntaxHelp exposing (ExposingKind)


rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.LinksPointToExistingPackageMembers" initialProjectContext
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = \_ _ -> fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { linksInReadme : Maybe (SourceAndLinks Rule.ReadmeKey)
    , linksInModules : List (SourceAndLinks Rule.ModuleKey)
    , exposed : EverySet SyntaxHelp.ModuleInfo
    }


type alias SourceAndLinks key =
    { key : key
    , links : EverySet LinkWithRange
    }


type alias LinkWithRange =
    { parsed : SyntaxHelp.Link
    , range : Range
    }


type alias ModuleContext =
    { docs : EverySet (Node String)
    , exposedFromModule : EverySet SyntaxHelp.ModuleInfo
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { exposed = EverySet.empty
    , linksInModules = []
    , linksInReadme = Nothing
    }


fromProjectToModule : ProjectContext -> ModuleContext
fromProjectToModule projectContext =
    { exposedFromModule = projectContext.exposed
    , docs = EverySet.empty
    }


fromModuleToProject : Rule.ModuleKey -> Node (List String) -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey (Node _ moduleName) { exposedFromModule, docs } =
    { linksInReadme = Nothing
    , exposed = exposedFromModule
    , linksInModules =
        [ { key = moduleKey
          , links =
                docs
                    |> EverySet.toList
                    |> List.concatMap
                        (\(Node { start } doc) ->
                            linksIn { doc = doc, start = start }
                        )
                    |> List.map (useIfNoModuleSpecified moduleName)
                    |> EverySet.fromList
          }
        ]
    }


useIfNoModuleSpecified :
    List String
    -> { range | parsed : SyntaxHelp.Link }
    -> { range | parsed : SyntaxHelp.Link }
useIfNoModuleSpecified moduleName ({ parsed } as link) =
    { link
        | parsed =
            case parsed.moduleName of
                [] ->
                    { parsed | moduleName = moduleName }

                _ ->
                    link.parsed
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposed = EverySet.union newContext.exposed previousContext.exposed
    , linksInModules = List.append newContext.linksInModules previousContext.linksInModules
    , linksInReadme = previousContext.linksInReadme
    }



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List nothing, ProjectContext )
readmeVisitor maybeReadme context =
    ( []
    , case maybeReadme of
        Just readme ->
            { context | linksInReadme = Just (findLinksInReadme readme) }

        Nothing ->
            context
    )



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { key_ | project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson context =
    ( []
    , { context
        | exposed =
            maybeElmJson
                |> Maybe.map exposedModulesInElmJson
                |> Maybe.withDefault EverySet.empty
      }
    )


exposedModulesInElmJson : { key_ | project : Project } -> EverySet SyntaxHelp.ModuleInfo
exposedModulesInElmJson { project } =
    case project of
        Project.Package { exposed } ->
            SyntaxHelp.exposedModules exposed
                |> EverySet.fromList
                |> EverySet.map
                    (\name ->
                        { moduleName =
                            name |> Module.toString |> String.split "."
                        , exposedDefinitions = ( SyntaxHelp.Explicit, [] )
                        }
                    )

        Project.Application _ ->
            EverySet.empty


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withCommentsVisitor commentsVisitor


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor (Node _ declaration) context =
    ( []
    , case SyntaxHelp.docOfDeclaration declaration of
        Just doc ->
            insertDoc context doc

        Nothing ->
            context
    )


commentsVisitor : List (Node String) -> ModuleContext -> ( List nothing, ModuleContext )
commentsVisitor comments context =
    ( []
    , comments
        |> find (Node.value >> SyntaxHelp.isFileComment)
        |> Maybe.map (insertDoc context)
        |> Maybe.withDefault context
    )


insertDoc : ModuleContext -> Node String -> ModuleContext
insertDoc context doc =
    { context | docs = EverySet.insert doc context.docs }


linksIn :
    { doc : String, start : Location }
    -> List LinkWithRange
linksIn { doc, start } =
    doc
        |> Parser.find SyntaxHelp.linkParser
        |> List.map
            (\link ->
                { link
                    | range =
                        { start = SyntaxHelp.addLocation start link.range.start
                        , end = SyntaxHelp.addLocation start link.range.end
                        }
                }
            )


findLinksInReadme : { readmeKey : Rule.ReadmeKey, content : String } -> SourceAndLinks Rule.ReadmeKey
findLinksInReadme readme =
    let
        { readmeKey, content } =
            readme
    in
    { key = readmeKey
    , links =
        linksIn
            { doc = content
            , start = { row = 1, column = 1 }
            }
            |> EverySet.fromList
    }


moduleDefinitionVisitor :
    Node Module
    -> ModuleContext
    -> ( List error_, ModuleContext )
moduleDefinitionVisitor (Node _ module_) context =
    ( []
    , { context
        | exposedFromModule =
            let
                info : SyntaxHelp.ModuleInfo
                info =
                    SyntaxHelp.moduleInfo module_
            in
            if
                context.exposedFromModule
                    |> EverySet.toList
                    |> List.any (\exposedElement -> exposedElement.moduleName == info.moduleName)
            then
                EverySet.insert info context.exposedFromModule

            else
                context.exposedFromModule
      }
    )


finalEvaluation : ProjectContext -> List (Rule.Error scope)
finalEvaluation context =
    let
        exposed : List SyntaxHelp.ModuleInfo
        exposed =
            context.exposed
                |> EverySet.toList

        exposedDict : Dict ModuleName ( ExposingKind, List String )
        exposedDict =
            exposed
                |> List.map (\{ moduleName, exposedDefinitions } -> ( moduleName, exposedDefinitions ))
                |> Dict.fromList

        exposedMembers : Set String
        exposedMembers =
            exposed
                |> List.concatMap
                    (\{ moduleName, exposedDefinitions } ->
                        let
                            moduleNameString : String
                            moduleNameString =
                                String.join "." moduleName

                            ( _, exposedDefs ) =
                                exposedDefinitions
                        in
                        moduleNameString
                            :: List.map
                                (\def -> moduleNameString ++ "." ++ def)
                                exposedDefs
                    )
                |> Set.fromList

        errorsForLinksInReadme : List (Rule.Error scope)
        errorsForLinksInReadme =
            context.linksInReadme
                |> Maybe.map (errorForLinkInReadme exposedDict exposedMembers)
                |> Maybe.withDefault []

        errorsForLinksInModules : List (Rule.Error scope)
        errorsForLinksInModules =
            List.concatMap (errorForLinkInModule exposedDict exposedMembers) context.linksInModules
    in
    List.append errorsForLinksInReadme errorsForLinksInModules


errorForLinkInReadme : Dict ModuleName ( ExposingKind, List String ) -> Set String -> SourceAndLinks Rule.ReadmeKey -> List (Rule.Error scope)
errorForLinkInReadme exposedDict exposedMembers { key, links } =
    links
        |> EverySet.toList
        |> List.filterMap
            (\link ->
                case ( link.parsed.moduleName, link.parsed.kind ) of
                    ( [], SyntaxHelp.DefinitionLink definition ) ->
                        Just
                            (Rule.errorForReadme key
                                (noModuleSpecifiedForDefinitionInLinkInReadme
                                    { badLink = definition
                                    , exposed = Set.toList exposedMembers
                                    }
                                )
                                link.range
                            )

                    _ ->
                        checkLink exposedDict exposedMembers (Rule.errorForReadme key) link
            )


errorForLinkInModule : Dict ModuleName ( ExposingKind, List String ) -> Set String -> SourceAndLinks Rule.ModuleKey -> List (Rule.Error scope)
errorForLinkInModule exposedDict exposedMembers { key, links } =
    links
        |> EverySet.toList
        |> List.filterMap (checkLink exposedDict exposedMembers (Rule.errorForModule key))


checkLink :
    Dict ModuleName ( ExposingKind, List String )
    -> Set String
    -> ({ message : String, details : List String } -> Range -> Rule.Error scope)
    -> LinkWithRange
    -> Maybe (Rule.Error scope)
checkLink exposedDict exposedMembers error link =
    case link.parsed.kind of
        SyntaxHelp.ModuleLink ->
            if Dict.member link.parsed.moduleName exposedDict then
                Nothing

            else
                Just
                    (error
                        { message = moduleInLinkNotExposed
                        , details = details exposedMembers link.parsed.moduleName
                        }
                        link.range
                    )

        SyntaxHelp.DefinitionLink definition ->
            case Dict.get link.parsed.moduleName exposedDict of
                Just exposedDefinitions ->
                    if SyntaxHelp.isExposed definition exposedDefinitions then
                        Nothing

                    else
                        Just
                            (error
                                { message = definitionInLinkNotExposedMessage
                                , details = details exposedMembers link.parsed.moduleName
                                }
                                link.range
                            )

                Nothing ->
                    -- TODO No corresponding module could be found, this should be a different error message
                    Just
                        (error
                            { message = definitionInLinkNotExposedMessage
                            , details = details exposedMembers link.parsed.moduleName
                            }
                            link.range
                        )


details : Set String -> ModuleName -> List String
details exposedMembers moduleNameParts =
    linkPointsToNonExistentMemberDetails
        { exposed = Set.toList exposedMembers
        , badLink = String.join "." moduleNameParts
        }


definitionInLinkNotExposedMessage : String
definitionInLinkNotExposedMessage =
    "The link points to a definition that isn't exposed from any exposed module in this package."


moduleInLinkNotExposed : String
moduleInLinkNotExposed =
    "The link points to a module that isn't listed in \"exposed-modules\"."


noModuleSpecifiedForDefinitionInLinkInReadme :
    { exposed : List String, badLink : String }
    -> { message : String, details : List String }
noModuleSpecifiedForDefinitionInLinkInReadme { exposed, badLink } =
    let
        moduleSuggestions : List String
        moduleSuggestions =
            exposed
                |> List.filter (String.endsWith badLink)
                |> List.map (String.dropRight (String.length badLink))
    in
    { message = "Using a link of the form [..](#definition) in the readme."
    , details =
        [ "There's no way to figure out in which module to look for this definition."
        , "I found `" ++ badLink ++ "` in the module(s) " ++ String.join ", " moduleSuggestions ++ "."
        ]
    }


linkPointsToNonExistentMemberDetails : { exposed : List String, badLink : String } -> List String
linkPointsToNonExistentMemberDetails { exposed, badLink } =
    let
        suggestions : String
        suggestions =
            exposed
                |> List.sortBy
                    (\member ->
                        -(JaroWinkler.similarity badLink member)
                    )
                |> List.take 3
                |> String.join ", "
    in
    [ "Links are only useful when they point to exposed package members."
    , "Maybe you meant one of those: " ++ suggestions ++ "."
    ]



-- Utils


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if predicate head then
                Just head

            else
                find predicate rest
