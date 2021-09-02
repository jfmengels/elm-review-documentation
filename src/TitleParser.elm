module TitleParser exposing (parser)

import Parser exposing ((|.), (|=), Parser)


{-| Taken from <https://github.com/dillonkearns/elm-markdown/blob/7.0.0/src/Markdown/Heading.elm>
then simplified and adapted to fit our needs

Copyright (c) 2019, Dillon Kearns

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Dillon Kearns nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}
parser : Parser String
parser =
    Parser.succeed identity
        |. (Parser.getChompedString Parser.spaces
                |> Parser.andThen
                    (\startingSpaces ->
                        let
                            startSpace : Int
                            startSpace =
                                String.length startingSpaces
                        in
                        if startSpace >= 4 then
                            Parser.problem "heading with < 4 spaces in front"

                        else
                            Parser.succeed startSpace
                    )
           )
        |. Parser.symbol "#"
        |. (Parser.getChompedString (Parser.chompWhile isHash)
                |> Parser.andThen
                    (\additionalHashes ->
                        let
                            level : Int
                            level =
                                String.length additionalHashes + 1
                        in
                        if level >= 7 then
                            Parser.problem "heading with < 7 #'s"

                        else
                            Parser.succeed level
                    )
           )
        |. Parser.oneOf
            [ Parser.symbol " "
            , Parser.symbol "\t"
            ]
        |= (Parser.chompWhile (always True)
                |> Parser.mapChompedString
                    (\headingText _ ->
                        headingText
                            |> String.trim
                            |> dropClosingSequence
                            |> toHtmlId
                    )
           )


toHtmlId : String -> String
toHtmlId =
    String.toLower


isHash : Char -> Bool
isHash c =
    case c of
        '#' ->
            True

        _ ->
            False


dropClosingSequence : String -> String
dropClosingSequence headingString =
    let
        droppedTrailingHashesString : String
        droppedTrailingHashesString =
            dropTrailingHashes headingString
    in
    if String.endsWith " " droppedTrailingHashesString || String.isEmpty droppedTrailingHashesString then
        String.trimRight droppedTrailingHashesString

    else
        headingString


dropTrailingHashes : String -> String
dropTrailingHashes headingString =
    if String.endsWith "#" headingString then
        dropTrailingHashes (String.dropRight 1 headingString)

    else
        headingString
