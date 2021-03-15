module Model exposing (..)

import Button exposing (Buttons, initialButtons)
import Plants as P exposing (Plant)
import Page exposing (Page(..))
import Page as Pa 
import Window exposing (..)




  ---- MODEL ---- 
type alias Model =
    { frame : Float   -- The current framecount of the app
    , window : Window -- The size of the game window
    , coins : Int     -- The number of coins the player currently has 
    , plants : List Plant -- The current list of a player's plants.
    , page : Page --The current game page
    , buttons : Buttons -- All current game buttons (accross all pages)
    , plotSize : PlotSize } -- The dimensions of a plot for rendering graphics

---- FLAGS ---- 
{-
  Flags pass values from js to elm
-}
type alias Flags =
    { width : Float
    , height : Float
    }

---- INITIALIZATION ----

{-
  Takes flags from JS and initializes the model
-}
initModel : Flags -> Model
initModel flag =
  let
    initPlants = P.initPlants
  in
    { frame = 0 
    , window = newWindow flag.width flag.height
    , coins = 5
    , plants = initPlants -- We define initial plants in Plants module
    , page = Farm
    , buttons = (initialButtons flag.width flag.height 150 120 initPlants) -- We define initialButtons in the Button module
    , plotSize = newPlotSize 150 120
    }