module ViewHelpers exposing (..)

import Html
import Html exposing (Html, text, div, br)
import Html.Attributes exposing (id, class)
import Html.Events as Events

-- Our Libraries
import Msg exposing (Msg(..))
import Plants as P
import Plants exposing (Plant)

---- Display Plants ----
-- DISPLAY FUNCTIONS -- 

{-
  Converts a Plant to an Html msg that can be displayed
  
  Args: 
    p - plant
  
  Returns:
    The visual representation of the plant.
-}
displayPlant : Plant -> Html msg
displayPlant p = 
  let
    classes = 
      if (p.age == 0)
      then [class "grown"]
      else []
  in
    div 
      ((class "plant")::classes)
      [ text ("Name: " ++ p.name ++ " ")
      , text ("Value: " ++ String.fromInt p.value ++ " ")
      , text ("Age: " ++ String.fromInt (p.matAge - p.age) ++ ".")]

{-
  Converts a list of plants to a viewable layout
  
  Args: 
    ps - list of plants
  
  Returns:
    The visual representation of all of a player's plants.
-}
plantsView : List Plant -> Html msg
plantsView ps =
  div 
  []
  (List.foldl (\n acc -> (displayPlant n)::acc) [] ps)


---- Store Display ---- 
{-
  Creates a buy button for a specific plant
  
  Args: 
    plant - plant
    price - price of that plant
  
  Returns:
    An Html purchase button for the plant.
-}
plantButton : Plant -> Int -> Html Msg
plantButton plant price =
  Html.button 
  [Events.onClick (BuyPlant plant price)]
  [text ("Buy " ++ plant.name ++ " for " ++ (String.fromInt price) ++ " Gold")]