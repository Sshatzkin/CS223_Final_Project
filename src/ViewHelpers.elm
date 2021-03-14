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
import String exposing (fromFloat)
import String exposing (fromInt)

---- Small Helpers ----

{-
  Takes numbr of frames, returns string format time (60 frames / second)
-}
frameToTime : Float -> String
frameToTime count =
  let
    seconds = (round count) // 60
    secString = 
      if ((modBy 60 seconds) < 10)
      then "0" ++ fromInt seconds
      else fromInt (modBy 60 seconds)
    mins = seconds // 60
    minString = 
      if (mins < 10)
      then "0" ++ fromInt mins
      else fromInt (modBy 60 mins)
    hours = mins // 60
    hourString = 
      if (hours < 10)
      then "0" ++ fromInt hours
      else fromInt hours
  in
    hourString ++ ":" ++ minString ++ ":" ++ secString

-- FARM DISPLAY FUNCTIONS -- 

{-
  Creates the Html page display of the player's farm and current plants

  Args:
    m - model

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
    ]


{-
  Creates a renderable for the canvas background
-}
renderBG : Model -> Canvas.Renderable
renderBG m = 
  -- Color.rgb255 128 255 235 is the original green. White would be 255 255 255.
  --  You can find more colors in the CSS file written in hex #.....
  shapes [ fill (Color.rgb255 128 255 235) ] [ rect ( 0, 0 ) m.window.width m.window.height ]

{-
  Displays the GUI for the Farm (header & coins)

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
           ( w.width / 2, w.height - (10))
           (" Coins = " ++ String.fromInt coins)
  ]


{-
  Produces a Canvas Renderable from a single plot button

  Args:
    b - button
    p - Plant

  Output:
    Canvas.Renderable of button
-}
renderPlot : Button -> Plant -> Canvas.Renderable
renderPlot b p =
  -- Want to render differently if purchased or not
  if p.purchased
  then
      let
        color = case p.ptype of
          Corn -> Color.yellow
          Tomato -> Color.red
          Pumpkin -> Color.orange
          Carrot -> Color.orange
          Raddish -> Color.purple
          Pepper ->
            case (modBy 4 (p.quantity - 1)) of
               0 -> Color.green
               1 -> Color.yellow
               2 -> Color.orange
               3 -> Color.red
               _ -> Color.green
        fillPercent = toFloat(p.matAge - p.countdown) / toFloat(p.matAge)
      in
        shapes [ fill color ] [ rect ( b.x, (b.height + b.y)) b.width (-1 * b.height * fillPercent) ]
    else shapes [ fill Color.gray ] [ rect ( b.x, b.y ) b.width b.height]

{-
  Produces a Canvas Renderable from a single upgrade button

  Args:
    b - button
    p - Plant

  Output:
    Canvas.Renderable of button
-}
renderUpgrade : Button -> Plant -> Canvas.Renderable
renderUpgrade b p = 
  if p.purchased
  then shapes [ fill Color.green ] [ rect ( b.x, b.y ) b.width b.height]
  else shapes [ fill Color.gray ] [ rect ( b.x, b.y ) b.width b.height]

{-
  Converts a list of buttons into a list of Canvas Renderables

  Args:
    bs - List of buttons
    ps - List of plants

  Output:
    Canvas.Renderable of button
-}
renderButtonList : List Button -> List Plant -> List (Canvas.Renderable)
renderButtonList bs ps =
  let
    foo b =
      case b.btype of
        Plot ptype -> 
          renderPlot b (P.getPlant ptype ps)
        Upgrade ptype -> 
          renderUpgrade b (P.getPlant ptype ps)
  in
    List.map foo bs

{-
  Produces a list of Canvas Renderables representing all buttons on the current page

  Args:
    m - Model

  Output:
    List of Canvas.Renderables of all buttons
-}
renderButtons : Model -> List (Canvas.Renderable)
renderButtons m =
  let
    -- Get list of buttons from current page
    buttonPage = (getButtonPage m.page m.buttons)
  in
    -- Render the list of buttons
    renderButtonList buttonPage m.plants