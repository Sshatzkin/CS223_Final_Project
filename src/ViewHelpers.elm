module ViewHelpers exposing (..)

import Html
import Html exposing (Html, text, div, br)
import Html.Attributes exposing (id, class)
import Html.Events as Events

-- Our Libraries
import Msg exposing (Msg(..))
import Plants as P
import Plants exposing (Plant)

---- Farm Display ----
-- DISPLAY FUNCTIONS -- 
{-
  Creates the page display of the player's farm and current plants

  Args:
    plants -- a list of the player's current plants
  
  Returns:
    An Html view of the farm page
-}
displayFarm : List Plant -> Html Msg
displayFarm plants =
  Html.div
    [ id "farm"]  -- The div's name is "farm"
    [ Html.span 
      []
      [ text "Welcome to Mr. Chickie's Farm! Grow plants and harvest them to earn money."
      ]
    , plantsView plants
    ]
{-
  Converts a Plant to an Html msg that can be displayed
  
  Args: 
    p - plant
  
  Returns:
    The visual representation of the plant.
-}
displayPlant : Plant -> Int -> Html Msg
displayPlant p index = 
  if (p.countdown == 0)
  then 
    -- Plant is grown
    div 
      [ class "plant"
      , class "grown"
      , Events.onClick (SellPlant index p)
      ]
      [ text ("Name: " ++ p.name ++ " ")
      , text ("Harvest for " ++ String.fromInt p.value ++ " gold.")
      ]
  else
    -- Not Grown
    div 
      [ class "plant"
      ]
      [ text ("Name: " ++ p.name ++ " ")
      , text ("Age: " ++ String.fromInt (p.matAge - p.countdown) ++ ".")
      ]

{-
  Converts a list of plants to a viewable layout
  
  Args: 
    ps - list of plants
  
  Returns:
    The visual representation of all of a player's plants.
-}
plantsView : List Plant -> Html Msg
plantsView ps =
  let
    views = List.indexedMap (\i p -> displayPlant p i) ps 
  in
    div 
    [id "plant_container"]
    (List.foldl (::) [] views)


---- Store Display ---- 
{-
  Puts together the entire store display for purchasing seeds

  Args:
    coins -- the player's current amount of money
  
  Returns:
    An Html view of the store page
-}
displayStore : Int -> Html Msg
displayStore coins =
  Html.div 
    [ id "store"]  -- The div's name is "store"
    [ Html.span 
      []
      [ text "Welcome to the seed store! Buy seeds to plant on your farm. "
      , text ("Coins: " ++ String.fromInt coins)
      ]
    , Html.button [Events.onClick (AddCoins 5)] [Html.text "Free Money"]
    , plantButton P.corn
    , plantButton P.pumpkin
    ]

{-
  Creates a buy button for a specific plant
  
  Args: 
    plant - plant
    price - price of that plant
  
  Returns:
    An Html purchase button for the plant.
-}
plantButton : Plant -> Html Msg
plantButton plant =
  Html.button 
  [Events.onClick (BuyPlant plant)]
  [text ("Buy " ++ plant.name ++ " for " ++ (String.fromInt plant.price) ++ " Gold")]