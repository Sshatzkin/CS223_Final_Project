module ViewHelpers exposing (..)

import Canvas exposing (rect, shapes, text)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Text exposing (font, align, TextAlign(..))
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html
import Html exposing (Html, div, br)
import Html.Attributes exposing (id, class)
import Html.Events as Events

-- Our Libraries
import Msg exposing (Msg(..))
import Page exposing (Page(..))
import Plants as P
import Plants exposing (Plant)

---- Type Definitions ----
type alias Window = 
  { width : Float 
  , height : Float
  }
newWindow : Float -> Float -> Window
newWindow w h = {width = w, height = h}

---- Farm Display with Canvas----
-- DISPLAY FUNCTIONS -- 
{-
  Creates the Html page display of the player's farm and current plants

  Args:
    w -- window from the model
    coins -- the player's current amount of money
    plants -- a list of the player's current plants
  
  Returns:
    An Html view of the farm page
-}
displayFarm : Window -> Int -> List (Plant) -> Html Msg
displayFarm w coins plants =
  div
  []
  [ Canvas.toHtml (truncate w.width, truncate w.height) 
      []
      ((displayFarmText w coins) ++ (renderPlants w plants))
  , Html.button [Events.onClick (ChangePage Store)] [Html.text "Go to Store"]
  ]
    
{-
  Html.div
    [ id "farm"]  -- The div's name is "farm"
    [ Html.span 
      []
      [ text "Welcome to Mr. Chickie's Farm! Grow plants and harvest them to earn money."
      , text ("Coins: " ++ String.fromInt coins)
      ]
    , plantsView plants
    , Html.button [Events.onClick (ChangePage Store)] [Html.text "Go to Store"]
    ]
-}


{-
  Displays the header message for the farm

  Args:
    w -- window from the model
    coins -- player's current coin total
  Output:
    list of Canvas Renderables
-}
displayFarmText : Window -> Int -> List (Canvas.Renderable)
displayFarmText w coins = 
  [ text [ font { size = 24, family = "monospace" }, align Center ]
           ( w.width / 2, w.height / 10 )
           " Welcome to Mr. Chickie's Farm!"
  , text [ font { size = 24, family = "monospace" }, align Center ] 
           ( w.width / 2, w.height / 6 )
           " Grow plants and harvest them to earn money. "
  , text [ font { size = 24, family = "monospace" }, align Center ]
           ( w.width / 2, w.height - (w.height / 10 ))
           (" Coins = " ++ String.fromInt coins)
  ]

{-
  Produces the image of a single plant as a Canvas Renderable
  Args:
    w -- current window in the model
    p -- Plant
  Output:
    Canvas.Renderable
-}
renderPlant : Window -> Plant -> Canvas.Renderable
renderPlant w p = 
  case p.name of
     "Corn" -> shapes [ fill Color.yellow ] [ rect ( w.width / 3, w.height / 3 ) 100 100 ]
     _ -> shapes [ fill Color.orange ] [ rect ( w.width - (w.width / 3), w.height / 3 ) 100 100 ]

{-
  Converts a list of plants into a list of Canvas Renderables
-}
renderPlants : Window -> List (Plant) -> List (Canvas.Renderable)
renderPlants w ps = 
  List.map (renderPlant w) ps

---OLD HTML DISPLAY FUNCTIONS---
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
    Canvas.toHtml ( 50, 50 )
      [ class "plant"
      , class "grown"
      , Events.onClick (SellPlant index p)
      ]
      --TODO: replace this with plant image
      [ text [] (25, 25) ("Name: " ++ p.name ++ " ")
      , text [] (30, 30) ("Harvest for " ++ String.fromInt p.value ++ " gold.")
      ]
  else
    -- Not Grown
    Canvas.toHtml ( 50, 50 ) 
      [ class "plant"
      ]
      [ text [] (25, 25) ("Name: " ++ p.name ++ " ")
      , text [] (30, 30) ("Age: " ++ String.fromInt (p.matAge - p.countdown) ++ ".")
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
      [ Html.text "Welcome to the seed store! Buy seeds to plant on your farm. "
      , Html.text ("Coins: " ++ String.fromInt coins)
      ]
    , Html.button [Events.onClick (AddCoins 5)] [Html.text "Free Money"]
    , plantButton P.corn
    , plantButton P.pumpkin
    , Html.button [Events.onClick (ChangePage Farm)] [Html.text "Go to Farm"]
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
  [Html.text ("Buy " ++ plant.name ++ " for " ++ (String.fromInt plant.price) ++ " Gold")]