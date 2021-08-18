module Docs.LinksPointToExistingPackageMembers.NotExposed exposing (definitionInLinkNotExposedMessage, linkPointsToNonExistentMemberDetails, moduleInLinkNotExposed, noModuleSpecifiedForDefinitionInLinkInReadme, rule)

import Elm.Module as Module
import Elm.Project as Project exposing (Project)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import EverySet as Set exposing (EverySet)
import JaroWinkler
import ParserExtra as Parser
import Review.Rule as Rule exposing (Rule)
import SyntaxHelp exposing (ExposingKind(..), Link, LinkKind(..), ModuleInfo, addLocation, docOfDeclaration, exposedModules, isExposed, isFileComment, linkParser, moduleInfo)


type alias Set a =
    EverySet a



--


rule : Rule
rule =
    Rule.newProjectRuleSchema "LinksPointToExistingPackageMembers" initialProjectContext
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> (let
                fromProjectToModule : ProjectContext -> ModuleContext
                fromProjectToModule context =
                    { exposed = context.exposed
                    , docs = Set.empty
                    }
            in
            Rule.withModuleContext
                { fromProjectToModule = \_ _ -> fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }
           )
        |> Rule.withFinalProjectEvaluation check
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { inReadme : Maybe (SourceAndLinks Rule.ReadmeKey)
    , inModules : List (SourceAndLinks Rule.ModuleKey)
    , exposed : Set ModuleInfo
    }


type alias SourceAndLinks key =
    { key : key
    , links : Set { parsed : Link, range : Range }
    }


type alias ModuleContext =
    { docs : Set (Node String)
    , exposed : Set ModuleInfo
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { exposed = Set.empty
    , inModules = []
    , inReadme = Nothing
    }


fromModuleToProject : Rule.ModuleKey -> Node (List String) -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey (Node _ moduleName) { exposed, docs } =
    { inReadme = Nothing
    , exposed = exposed
    , inModules =
        [ { key = moduleKey
          , links =
                docs
                    |> Set.toList
                    |> List.concatMap
                        (\(Node { start } doc) ->
                            linksIn { doc = doc, start = start }
                        )
                    |> List.map (useIfNoModuleSpecified moduleName)
                    |> Set.fromList
          }
        ]
    }


useIfNoModuleSpecified :
    List String
    -> { range | parsed : Link }
    -> { range | parsed : Link }
useIfNoModuleSpecified moduleName ({ parsed } as match) =
    { match
        | parsed =
            case parsed.moduleName of
                [] ->
                    { parsed | moduleName = moduleName }

                _ ->
                    match.parsed
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposed = Set.union newContext.exposed previousContext.exposed
    , inModules = List.append newContext.inModules previousContext.inModules
    , inReadme = previousContext.inReadme
    }



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List nothing, ProjectContext )
readmeVisitor maybeReadme context =
    ( []
    , case maybeReadme of
        Just readme ->
            { context | inReadme = Just (linksInReadme readme) }

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
                |> Maybe.withDefault Set.empty
      }
    )


exposedModulesInElmJson : { key_ | project : Project } -> Set ModuleInfo
exposedModulesInElmJson { project } =
    case project of
        Project.Package { exposed } ->
            exposedModules exposed
                |> Set.fromList
                |> Set.map
                    (\name ->
                        { moduleName =
                            name |> Module.toString |> String.split "."
                        , exposedDefinitions = ( Explicit, [] )
                        }
                    )

        Project.Application _ ->
            Set.empty


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor exposedInModule
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withCommentsVisitor commentsVisitor


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor (Node _ declaration) context =
    ( []
    , case docOfDeclaration declaration of
        Just doc ->
            insertDoc context doc

        Nothing ->
            context
    )


commentsVisitor : List (Node String) -> ModuleContext -> ( List nothing, ModuleContext )
commentsVisitor comments context =
    ( []
    , comments
        |> find (Node.value >> isFileComment)
        |> Maybe.map (insertDoc context)
        |> Maybe.withDefault context
    )


insertDoc : ModuleContext -> Node String -> ModuleContext
insertDoc context doc =
    { context | docs = Set.insert doc context.docs }


linksIn :
    { doc : String, start : Location }
    -> List { parsed : Link, range : Range }
linksIn { doc, start } =
    doc
        |> Parser.find linkParser
        |> List.map
            (\match ->
                { match
                    | range =
                        let
                            inComment =
                                addLocation start
                        in
                        { start = inComment match.range.start
                        , end = inComment match.range.end
                        }
                }
            )


linksInReadme :
    { readmeKey : Rule.ReadmeKey, content : String }
    -> { key : Rule.ReadmeKey, links : Set { parsed : Link, range : Range } }
linksInReadme readme =
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
            |> Set.fromList
    }


exposedInModule :
    Node Module
    -> ModuleContext
    -> ( List error_, ModuleContext )
exposedInModule (Node _ module_) context =
    ( []
    , { context
        | exposed =
            let
                info =
                    module_ |> moduleInfo
            in
            if
                context.exposed
                    |> Set.toList
                    |> List.any
                        (.moduleName
                            >> (==) info.moduleName
                        )
            then
                context.exposed |> Set.insert info

            else
                context.exposed
      }
    )


check : ProjectContext -> List (Rule.Error e_)
check { inReadme, exposed, inModules } =
    let
        exposedMembers : Set String
        exposedMembers =
            exposed
                |> Set.toList
                |> List.concatMap
                    (\{ moduleName, exposedDefinitions } ->
                        let
                            moduleNameString =
                                moduleName |> String.join "."

                            ( _, exposedDefs ) =
                                exposedDefinitions
                        in
                        exposedDefs
                            |> List.map
                                (\def ->
                                    moduleNameString ++ "." ++ def
                                )
                            |> (::) moduleNameString
                    )
                |> Set.fromList

        details : ModuleName -> List String
        details moduleNameParts =
            linkPointsToNonExistentMemberDetails
                { exposed = Set.toList exposedMembers
                , badLink = String.join "." moduleNameParts
                }

        checkLink error match =
            let
                { moduleName, kind } =
                    match.parsed
            in
            case kind of
                ModuleLink ->
                    if
                        exposed
                            |> Set.toList
                            |> List.any
                                (.moduleName
                                    >> (==) moduleName
                                )
                    then
                        []

                    else
                        [ error
                            { message = moduleInLinkNotExposed
                            , details = details moduleName
                            }
                            match.range
                        ]

                DefinitionLink definition ->
                    if
                        exposed
                            |> Set.toList
                            |> List.any
                                (\m ->
                                    (m.moduleName == moduleName)
                                        && (m.exposedDefinitions
                                                |> isExposed definition
                                           )
                                )
                    then
                        []

                    else
                        [ error
                            { message = definitionInLinkNotExposedMessage
                            , details = details moduleName
                            }
                            match.range
                        ]
    in
    [ inReadme
        |> Maybe.map
            (\{ key, links } ->
                links
                    |> Set.toList
                    |> List.concatMap
                        (\match ->
                            case ( match.parsed.moduleName, match.parsed.kind ) of
                                ( [], DefinitionLink definition ) ->
                                    [ Rule.errorForReadme key
                                        (noModuleSpecifiedForDefinitionInLinkInReadme
                                            { badLink = definition
                                            , exposed = exposedMembers |> Set.toList
                                            }
                                        )
                                        match.range
                                    ]

                                _ ->
                                    checkLink (Rule.errorForReadme key) match
                        )
            )
        |> Maybe.withDefault []
    , inModules
        |> List.concatMap
            (\{ key, links } ->
                links
                    |> Set.toList
                    |> List.concatMap
                        (checkLink (Rule.errorForModule key))
            )
    ]
        |> List.concat


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
        , "I found `" ++ badLink ++ "` in the nodule(s) " ++ String.join ", " moduleSuggestions ++ "."
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
