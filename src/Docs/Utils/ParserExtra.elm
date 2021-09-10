module Docs.Utils.ParserExtra exposing (find, manySeparated)

import Parser exposing ((|.), (|=), Parser)


{-| `range` is relative to the string start (so 0,0).
-}
find : Parser a -> String -> List a
find parser string =
    string
        |> Parser.run (findParser parser)
        |> Result.withDefault []


findParser : Parser a -> Parser (List a)
findParser parser =
    Parser.loop []
        (\parsed ->
            Parser.oneOf
                [ Parser.succeed (\p -> p :: parsed)
                    |= parser
                    |> Parser.map Parser.Loop
                , Parser.succeed parsed
                    |. Parser.chompIf (\_ -> True)
                    |> Parser.map Parser.Loop
                , Parser.end
                    |> Parser.map (\() -> Parser.Done (List.reverse parsed))
                ]
        )


{-| 0 or more things directly separated by a string like "go-gi-ga".
-}
manySeparated :
    { by : String
    , item : Parser between
    }
    -> Parser (List between)
manySeparated { by, item } =
    Parser.sequence
        { start = ""
        , separator = by
        , end = ""
        , spaces = Parser.symbol ""
        , item = item
        , trailing = Parser.Forbidden
        }
