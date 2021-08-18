module Docs.LinksPointToExistingPackageMembers exposing (rule)

import Dict exposing (Dict)
import Elm.Module
import Elm.Project as Project exposing (Project)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import JaroWinkler
import ParserExtra as Parser
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import SyntaxHelp exposing (ExposedDefinitions)


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


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withCommentsVisitor commentsVisitor


type alias ProjectContext =
    { linksInReadme : Maybe (SourceAndLinks Rule.ReadmeKey)
    , linksInModules : List (SourceAndLinks Rule.ModuleKey)
    , exposedModules : Set ModuleName
    , exposed : List SyntaxHelp.ModuleInfo
    }


type alias SourceAndLinks key =
    { key : key
    , links : List LinkWithRange
    }


type alias LinkWithRange =
    { parsed : SyntaxHelp.Link
    , range : Range
    }


type alias ModuleContext =
    { docs : List (Node String)
    , exposedModules : Set ModuleName
    , exposedFromModule : List SyntaxHelp.ModuleInfo
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { exposed = []
    , exposedModules = Set.empty
    , linksInModules = []
    , linksInReadme = Nothing
    }


fromProjectToModule : ProjectContext -> ModuleContext
fromProjectToModule projectContext =
    { exposedFromModule = []
    , exposedModules = projectContext.exposedModules
    , docs = []
    }


fromModuleToProject : Rule.ModuleKey -> Node (List String) -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey (Node _ moduleName) { exposedFromModule, docs } =
    { linksInReadme = Nothing
    , exposed = exposedFromModule
    , exposedModules = Set.empty
    , linksInModules =
        [ { key = moduleKey
          , links =
                docs
                    |> List.concatMap
                        (\(Node { start } doc) ->
                            linksIn { doc = doc, start = start }
                        )
                    |> List.map (useIfNoModuleSpecified moduleName)
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
    { exposed = List.append newContext.exposed previousContext.exposed
    , linksInModules = List.append newContext.linksInModules previousContext.linksInModules
    , exposedModules = previousContext.exposedModules
    , linksInReadme = previousContext.linksInReadme
    }



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List nothing, ProjectContext )
readmeVisitor maybeReadme context =
    ( []
    , case maybeReadme of
        Just readme ->
            { context
                | linksInReadme =
                    Just
                        { key = readme.readmeKey
                        , links = findLinksInReadme readme.content
                        }
            }

        Nothing ->
            context
    )



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { key_ | project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson context =
    ( []
    , { context
        | exposedModules =
            case maybeElmJson of
                Just elmJson ->
                    exposedModulesInElmJson elmJson

                Nothing ->
                    Set.empty
      }
    )


exposedModulesInElmJson : { key_ | project : Project } -> Set ModuleName
exposedModulesInElmJson { project } =
    case project of
        Project.Package { exposed } ->
            SyntaxHelp.exposedModules exposed
                |> List.map (\name -> name |> Elm.Module.toString |> String.split ".")
                |> Set.fromList

        Project.Application _ ->
            Set.empty


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor (Node _ declaration) context =
    ( []
    , case SyntaxHelp.docOfDeclaration declaration of
        Just doc ->
            { context | docs = doc :: context.docs }

        Nothing ->
            context
    )


commentsVisitor : List (Node String) -> ModuleContext -> ( List nothing, ModuleContext )
commentsVisitor comments context =
    ( []
    , case find (Node.value >> SyntaxHelp.isFileComment) comments of
        Just doc ->
            { context | docs = doc :: context.docs }

        Nothing ->
            context
    )


linksIn : { doc : String, start : Location } -> List LinkWithRange
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


findLinksInReadme : String -> List LinkWithRange
findLinksInReadme content =
    linksIn
        { doc = content
        , start = { row = 1, column = 1 }
        }


moduleDefinitionVisitor :
    Node Module
    -> ModuleContext
    -> ( List error_, ModuleContext )
moduleDefinitionVisitor (Node _ module_) context =
    let
        info : SyntaxHelp.ModuleInfo
        info =
            SyntaxHelp.moduleInfo module_
    in
    ( []
    , { context
        | exposedFromModule =
            if Set.member (Module.moduleName module_) context.exposedModules then
                info :: context.exposedFromModule

            else
                context.exposedFromModule
      }
    )


finalEvaluation : ProjectContext -> List (Rule.Error scope)
finalEvaluation context =
    let
        exposedDict : Dict ModuleName ( ExposedDefinitions, List String )
        exposedDict =
            context.exposed
                |> List.map (\{ moduleName, exposedDefinitions } -> ( moduleName, exposedDefinitions ))
                |> Dict.fromList

        exposedMembers : Set String
        exposedMembers =
            context.exposed
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


errorForLinkInReadme : Dict ModuleName ( ExposedDefinitions, List String ) -> Set String -> SourceAndLinks Rule.ReadmeKey -> List (Rule.Error scope)
errorForLinkInReadme exposedDict exposedMembers { key, links } =
    List.filterMap
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
        links


errorForLinkInModule : Dict ModuleName ( ExposedDefinitions, List String ) -> Set String -> SourceAndLinks Rule.ModuleKey -> List (Rule.Error scope)
errorForLinkInModule exposedDict exposedMembers { key, links } =
    List.filterMap (checkLink exposedDict exposedMembers (Rule.errorForModule key)) links


checkLink :
    Dict ModuleName ( ExposedDefinitions, List String )
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
