module Components.TimeLabel exposing (timeLabel)

-- vendor
import Html exposing (Html, text)
import Date.Extra.Period as Period exposing (DeltaRecord)
import Date.Extra.Core as XDate
import Date exposing (Date)


accessors : List (String, (DeltaRecord -> Int))
accessors =
  [ ("w", .week)
  , ("d", .day)
  , ("h", .hour)
  , ("m", .minute)
  ]


maybeMappers : List (DeltaRecord -> Maybe String)
maybeMappers =
  let
    mapper = \(t, f) -> toMaybe t f
  in
    List.map mapper accessors


toMaybe : String -> (DeltaRecord -> Int) -> DeltaRecord -> Maybe String
toMaybe letter fun record =
  let
    val = fun record
  in
    if val > 0 then
      Just <| toString val ++ letter
    else
      Nothing


timeLabel : Date -> Int -> Html a
timeLabel pastDate currentTime =
  let
    currentDate = XDate.fromTime (currentTime * 1000)
    diff = Period.diff pastDate currentDate
    _ = Debug.log "diff" diff
    mbTimeLbl = Maybe.oneOf <| List.map (\mapper -> mapper diff) maybeMappers
  in
    text <| Maybe.withDefault "now" mbTimeLbl
