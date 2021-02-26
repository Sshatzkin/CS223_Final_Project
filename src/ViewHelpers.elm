module ViewHelpers exposing (..)

import Html
import Html exposing (Html, text, div, br)
import Html.Attributes exposing (id)
import Html.Events as Events

-- Our Libraries
import Msg exposing (Msg(..))
import Plants as P
import Plants exposing (Plant)

plantButton : Plant -> Int -> Html Msg
plantButton plant price =
  Html.button 
  [Events.onClick (BuyPlant plant price)]
  [text ("Buy " ++ plant.name ++ " for " ++ (String.fromInt price) ++ " Gold")]