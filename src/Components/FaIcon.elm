module Components.FaIcon exposing (..)

-- vendor
import Html exposing (Html, i)
import Html.Attributes exposing (class)


faIcon : String -> Html a
faIcon name =
  i [ class <| "fa fa-" ++ name ] []
