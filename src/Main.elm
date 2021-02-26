module Main exposing (..)

import Browser
import Browser.Events as BEvents
import Json.Decode as Decode
import Html
import Html exposing (Html, text)
import Html.Attributes exposing (id)
import Html.Events as Events

-- Our Libraries
import Msg exposing (Msg(..))
import Plants as P
import Plants exposing (Plant)
import ViewHelpers as VH


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
    }

type alias Window = 
  { height : Float
  , width : Float 
  }


initModel : Flags -> Model
initModel flag =
    { frame = 0 
    , window = 
      { width = flag.width  -- This is where we use flags to set the model
      , height = flag.height}
    , coins = 0
    , plants = P.initPlants -- We define initial plants in Plants module
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

      Increment ->
      -- Called to increment player coins by 1
        ( { model | coins = model.coins + 1 }, Cmd.none )

      BuyPlant p cost ->
      -- Called by plant purchase buttons to initiate plant purchase
        ({ model | plants = (P.addPlant p model.plants), coins = model.coins - cost}, Cmd.none)

-- VIEW
{-
  Called by the main to create the Html view.
-}
view : Model -> Html Msg
view model =
    Html.div  -- We need a containing div for all of these items
        [ id "game"]  -- The div's name is "game"
        [ Html.span 
          []
          [ text "Welcome to our game! Click to earn coins press esc. to decrement."
          , text ("Coins: " ++ String.fromInt model.coins)
          ]
        , Html.button [Events.onClick Increment] [Html.text "Free Money"]
        , VH.plantButton P.corn 1
        , VH.plantButton P.pumpkin 5
        , P.plantsView model.plants
        , Html.span 
          [ id "frameRate"] 
          [ text ("Frame: " ++ String.fromFloat model.frame)]
        ]