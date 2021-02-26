module Main exposing (..)

import Browser
import Browser.Events as BEvents
import Json.Decode as Decode
import Html
import Html exposing (Html, text)
import Html.Attributes exposing (id)

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
    , count : Int 
    , player : Player}

type alias Window = 
  { height : Float
  , width : Float 
  }

type alias Plant = 
  {
    growth : Int
  , value : Int 
  }

type alias Player = 
  { coins : Int
  , experience : Int}

initModel : Flags -> Model
initModel flag =
    { frame = 0
    , window = 
      { width = flag.width 
      , height = flag.height}
    , count = 0
    , player = 
      { coins = 0
      , experience = 0 } 
    }


init : Flags -> ( Model, Cmd Msg )
init flag =
    ( initModel flag, Cmd.none )


-- UPDATE
type Msg
    = NoOp
    | Reset
    | Increment
    | Frame Float

keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BEvents.onAnimationFrameDelta Frame
        , BEvents.onClick (Decode.succeed Increment)
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
          ({model | frame = model.frame + 1 }, Cmd.none)
        NoOp ->
            ( model, Cmd.none )
        Reset ->
            ({model | count = 0}, Cmd.none )

        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )



-- VIEW
view : Model -> Html Msg
view model =
    Html.div
        [ id "game"]
        [ Html.span 
          []
          [ text "Welcome to our game! Click to increment, press esc. to decrement."
          , text ("Clicks: " ++ String.fromInt model.count)
          , text ("Coins: " ++ String.fromInt model.player.coins)
          ]
        , Html.span 
          [ id "frameRate"] 
          [ text ("Frame: " ++ String.fromFloat model.frame)]
        ]
