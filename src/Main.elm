module Main exposing (..)

import Browser
import Browser.Events as BEvents
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html exposing (Html, text)
import Html.Attributes exposing (id)
import Html.Events as Events

-- Our Libraries
import Msg exposing (Msg(..))
import Plants as P
import Plants exposing (Plant)
import ViewHelpers as VH
import Html

-- TODO - Look into canvas to decide how we want to do visual

-- TODO FIGURE OUT HOW TO DO LOCAL STORAGE THANKS

-- TODO TOGGLE BETWEEN PAGES

{-
  The main of the entire application
-}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

---- FLAGS ---- 

{-
  Flags pass values from js to elm
-}
type alias Flags =
    { width : Float
    , height : Float
    }


  ---- MODEL ---- 
type alias Model =
    { frame : Float   -- The current framecount of the app
    , window : Window -- The size of the game window
    , coins : Int     -- The number of coins the player currently has 
    , plants : List Plant -- The current list of a player's plants.
    , page : Page --The current game page
    }

type alias Window = 
  { height : Float
  , width : Float 
  }
type Page = Farm | Store

initModel : Flags -> Model
initModel flag =
    { frame = 0 
    , window = 
      { width = flag.width  -- This is where we use flags to set the model
      , height = flag.height}
    , coins = 5
    , plants = P.initPlants -- We define initial plants in Plants module
    , page = Store
    }

{-
  This function is called by the main on start
-}
init : Flags -> ( Model, Cmd Msg )
init flag =
    ( initModel flag, Cmd.none )


---- UPDATE ----

{-
  This is used to detect key strokes
-}
keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

{-
  This is called by main to establish subscriptions.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BEvents.onAnimationFrameDelta Frame   -- This sub updates on every frame (this is our frame cycle)
        , BEvents.onClick (Decode.succeed NoOp) -- This sub updates on click
        , BEvents.onKeyDown   -- This sub updates on key down - case for key
            (Decode.map
                (\key ->
                    if key == "Escape" then
                        Reset

                    else
                        NoOp
                )
                (Decode.field "key" Decode.string)
            )
        ]

{-
  This function is called by main every time a new Msg is sent. Case for Msg.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Frame _ ->  
      -- Called on every animation frame
        ({model | frame = model.frame + 1, plants = P.agePlants model.plants }, Cmd.none)

      NoOp ->   
      -- Called when we need a "NoOp"
        ( model, Cmd.none )

      Reset -> 
      -- Called to reset player coins
        ({ model | coins = 0}, Cmd.none )

      BuyPlant p ->
      -- Called by plant purchase buttons to initiate plant purchase
        if (p.price > model.coins)
        then
          (model, Cmd.none)
        else
          ({ model | plants = (P.addPlant p model.plants), coins = model.coins - p.price}, Cmd.none)
      
      SellPlant index plant ->
      -- Called when a plant is harvested
        ({model | coins = model.coins + (P.value plant), plants = P.removePlant index model.plants}, Cmd.none) 

      AddCoins val ->
      -- Called to add or subtract a given value from the player's total money
        ( { model | coins = model.coins + val }, Cmd.none)

-- VIEW
{-
  Called by the main to create the Html view.
-}

view : Model -> Html Msg
view model =
  Html.div
    [ id "game"]
    [ gameView model
    , Html.span 
          [ id "frameRate"] 
          [ text ("Frame: " ++ String.fromFloat model.frame)]
    ]

{-
    Html.div  -- We need a containing div for all of these items
        [ id "game"]  -- The div's name is "game"
        [ Html.span 
          []
          [ text "Welcome to our game! Click to earn coins press esc. to decrement."
          , text ("Coins: " ++ String.fromInt model.coins)
          ]
        , Html.button [Events.onClick (AddCoins 5)] [Html.text "Free Money"]
        , VH.plantButton P.corn
        , VH.plantButton P.pumpkin
        , VH.plantsView model.plants
        , Html.span 
          [ id "frameRate"] 
          [ text ("Frame: " ++ String.fromFloat model.frame)]
        ]
-}

{-
  Determines which game page should be displayed
-}
gameView : Model -> Html Msg
gameView model = 
  case model.page of
          Farm -> VH.displayFarm model.plants
          Store -> VH.displayStore model.coins