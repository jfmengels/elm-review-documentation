module LinkParserTest exposing (suite)

import Expect
import Parser
import SyntaxHelp exposing (LinkKind(..), linkParser)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "linkParser"
        [ describe "succeeds"
            [ test "definition link 0"
                (\() ->
                    "[iqrefh932hfub`](A-B-C#gogey)"
                        |> Parser.run linkParser
                        |> Expect.equal
                            (Ok
                                { kind = DefinitionLink "gogey"
                                , moduleName = [ "A", "B", "C" ]
                                }
                            )
                )
            , test "definition link 1"
                (\() ->
                    "[`b`](B#b)"
                        |> Parser.run linkParser
                        |> Expect.equal
                            (Ok
                                { kind = DefinitionLink "b"
                                , moduleName = [ "B" ]
                                }
                            )
                )
            , test "alias definition link"
                (\() ->
                    "[`Error`](B#Error)"
                        |> Parser.run linkParser
                        |> Expect.equal
                            (Ok
                                { kind = DefinitionLink "Error"
                                , moduleName = [ "B" ]
                                }
                            )
                )
            , test "definition link without module name"
                (\() ->
                    "[`a`](#a)"
                        |> Parser.run linkParser
                        |> Expect.equal
                            (Ok
                                { kind = DefinitionLink "a"
                                , moduleName = []
                                }
                            )
                )
            , test "module link 0"
                (\() ->
                    "[`A.And.B`](A-And-B)"
                        |> Parser.run linkParser
                        |> Expect.equal
                            (Ok
                                { kind = ModuleLink
                                , moduleName = [ "A", "And", "B" ]
                                }
                            )
                )
            , test "module link 1"
                (\() ->
                    "[`A`](A)"
                        |> Parser.run linkParser
                        |> Expect.equal
                            (Ok
                                { kind = ModuleLink
                                , moduleName = [ "A" ]
                                }
                            )
                )
            ]
        ]
