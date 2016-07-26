import Html exposing (text, div)
import String
import Json.Decode.Extra exposing ((|:), lazy)
import Json.Decode as Json exposing
  ( Decoder
  , string
  , bool
  , int
  , list
  , succeed
  , oneOf
  , map
  , andThen
  , (:=)
  , maybe
  , decodeString
  , customDecoder
  , value
  , decodeValue
  )


type Item
  = Lite Int
  | Full ItemData


type ItemType
  = Job
  | Story
  | Comment
  | Poll
  | Pollopt
  | Unknown

jsonString2 =
  """
  {
    "by":"vasili111",
    "descendants":0,
    "id":12120160,
    "score":7,
    "time":1468909591,
    "title":"Functional programming in C",
    "type":"story",
    "kids" : [ 8952, 9224, 8917, 8884, 8887, 8943, 8869
             , 8958, 9005, 9671, 8940, 9067, 8908, 9055
             , 8865, 8881, 8872, 8873, 8955, 10403, 8903
             , 8928, 9125, 8998, 8901, 8902, 8907, 8894
             , 8878, 8870, 8980, 8934, 8876 ],
    "url":"https://lucabolognese.wordpress.com/2013/01/04/functional-programming-in-c/"
  }
  """
jsonString =
  """
  [
    {
      "by" : "dhouston",
      "descendants" : 71,
      "id" : 8863,
      "score" : 111,
      "parent": 2133,
      "time" : 1175714200,
      "title" : "My YC app: Dropbox - Throw away your USB drive",
      "type" : "story",
      "url" : "http://www.getdropbox.com/u/2/screencast.html",
      "kids" : [ 8952, 9224, 8917, 8884, 8887, 8943, 8869
               , 8958, 9005, 9671, 8940, 9067, 8908, 9055
               , 8865, 8881, 8872, 8873, 8955, 10403, 8903
               , 8928, 9125, 8998, 8901, 8902, 8907, 8894
               , 8878, 8870, 8980, 8934, 8876 ]
    },
    8864
  ]
  """


type alias ItemData =
  { id: Int
  , deleted: Bool
  , kind: ItemType
  , by: String
  , time: Int
  , text: Maybe String
  , parent: Maybe Item
  , kids: Maybe (List Item)
  , url: Maybe String
  , score: Maybe Int
  , title: Maybe String
  , parts: Maybe (List Item)
  , descendants: Maybe Int
  }


items : Decoder (List Item)
items =
  list item


item : Decoder Item
item =
  oneOf
    [ map Lite int
    , map Full full
    ]


full : Decoder ItemData
full =
  lazy <| \_ ->
    map ItemData ("id"      := int)
      |: oneOf [ "deleted"  := bool, succeed False ]
      |: ("type"            := itemType)
      |: ("by"              := string)
      |: ("time"            := int)
      |: ("text"            := string      |> maybe)
      |: ("parent"          := item        |> maybe)
      |: ("kids"            := (list item) |> maybe)
      |: ("url"             := string      |> maybe)
      |: ("score"           := int         |> maybe)
      |: ("title"           := string      |> maybe)
      |: ("parts"           := (list item) |> maybe)
      |: ("descendants"     := int         |> maybe)


itemType : Decoder ItemType
itemType =
  let
    stringToItemKind typeName =
      case typeName of
        "job" ->
          Job

        "story" ->
          Story

        "comment" ->
          Comment

        "poll" ->
          Poll

        "pollopt" ->
          Pollopt

        _ ->
          Unknown

    convert typeName =
      stringToItemKind typeName |> succeed
  in
    string `andThen` convert


itemz = Debug.log "item" <| decodeString item jsonString2


main =
  case itemz of
    Ok value ->
      div [] [ text <| toString value ]

    Err txt ->
      div [] [ text txt ]
