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

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

---- Flags ---- 
type alias Flags =
    { width : Float
    , height : Float
    }


  ---- MODEL ---- 
type alias Model =
    { frame : Float
    , window : Window
    , coins : Int
    , plants : List Plant}

type alias Window = 
  { height : Float
  , width : Float 
  }

initModel : Flags -> Model
initModel flag =
    { frame = 0
    , window = 
      { width = flag.width 
      , height = flag.height}
    , coins = 0
    , plants = P.initPlants 
    }


init : Flags -> ( Model, Cmd Msg )
init flag =
    ( initModel flag, Cmd.none )


-- UPDATE
keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BEvents.onAnimationFrameDelta Frame
        , BEvents.onClick (Decode.succeed NoOp)
        , BEvents.onKeyDown
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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ = 1
    in
    case msg of
        Frame _ ->
          ({model | frame = model.frame + 1, plants = P.agePlants model.plants }, Cmd.none)
        NoOp ->
          ( model, Cmd.none )
        Reset ->
          ({ model | coins = 0}, Cmd.none )

        Increment ->
          ( { model | coins = model.coins + 1 }, Cmd.none )
        BuyPlant p cost ->
          ({ model | plants = (P.addPlant p model.plants), coins = model.coins - cost}, Cmd.none)



-- VIEW
view : Model -> Html Msg
view model =
    Html.div
        [ id "game"]
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
