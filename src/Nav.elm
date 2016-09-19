module Nav exposing (..)

-- vendor

import Navigation
import UrlParser exposing (Parser, oneOf, format, (</>), int, s)
import String


-- local

import Model exposing (Page(..))


toHash : Page -> String
toHash page =
    case page of
        HomePage ->
            "#home"

        StoryPage id ->
            "#story/" ++ toString id


hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format HomePage (s "home")
        , format StoryPage (s "story`" </> int)
        ]


parser : Navigation.Parser (Result String Page)
parser =
    (Navigation.makeParser hashParser)
