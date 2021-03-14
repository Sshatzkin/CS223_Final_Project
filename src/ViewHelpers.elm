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
import Html.Events.Extra.Mouse as Mouse

-- Our Libraries
import Window exposing (Window)
import Msg exposing (Msg(..))
import Model exposing (Model)
import Page exposing (Page(..))
import Plants as P
import Plants exposing (Plant, PType (..))
import Button exposing (BType (..), Button, Buttons, getButtonPage)



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
displayFarm : Model -> Html Msg
displayFarm m =
  let
    frame = m.frame
    w = m.window
    coins = m.coins
    plants = m.plants
  in
    div
    []
    [ Canvas.toHtml (truncate w.width, truncate w.height) 
      [Mouse.onClick Click]
      ((renderBG m)::(displayFarmText w coins) ++ (renderButtons m))
    , Html.button [Events.onClick (ChangePage Store)] [Html.text "Go to Store"]
    ]


{-
  Creates a renderable for the background

-}
renderBG : Model -> Canvas.Renderable
renderBG m = 
  shapes [ fill {- #80ffdb-} (Color.rgb255 128 255 235) ] [ rect ( 0, 0 ) m.window.width m.window.height ]

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
  Produces the image of a single plot as a Canvas Renderable
  Args:
    b -- current button
    p -- Plant
  Output:
    Canvas.Renderable
-}
renderPlot : Button -> Plant -> Canvas.Renderable
renderPlot b p = 
  let
    color = case p.ptype of
      Corn -> Color.yellow
      Tomato -> Color.red
      Pumpkin -> Color.orange
      Carrot -> Color.orange
    fillPercent = toFloat(p.matAge - p.countdown) / toFloat(p.matAge)
  in
    shapes [ fill color ] [ rect ( b.x, (b.height + b.y)) b.width (-1 * b.height * fillPercent) ]

{-
  Converts a list of plants into a list of Canvas Renderables
-}
renderPlots : List Button -> List Plant -> List (Canvas.Renderable)
renderPlots buttons ps =
  let
    foo b =
      case b.btype of
        Plot ptype -> 
          case (P.getPlant ptype ps) of
            Nothing -> Debug.todo "Could not find plant to render"
            Just p -> renderPlot b p
        _ -> Debug.todo "This case should not occur"
  in
    List.map foo buttons

renderButtons : Model -> List (Canvas.Renderable)
renderButtons m =
  let
    buttonPage = (getButtonPage m.page m.buttons)
  in
    renderPlots 
      (List.filter 
        (\ b -> case b.btype of 
                  Plot _ -> True 
                  _ -> False) 
        buttonPage) 
      m.plants









------------------------------------------------------------------
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